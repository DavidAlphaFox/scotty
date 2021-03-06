{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Scotty.Internal.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT)
import           Control.Monad.Trans.Except

import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default.Class (Default, def)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mempty)
#endif
import           Data.String (IsString(..))
import           Data.Text.Lazy (Text, pack)
import           Data.Typeable (Typeable)

import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration)
import           Network.Wai.Parse (FileInfo)

--------------------- Options -----------------------
data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: Settings -- ^ Warp 'Settings'
                                              -- Note: to work around an issue in warp,
                                              -- the default FD cache duration is set to 0
                                              -- so changes to static files are always picked
                                              -- up. This likely has performance implications,
                                              -- so you may want to modify this for production
                                              -- servers using `setFdCacheDuration`.
                       }

instance Default Options where
    def = Options 1 (setFdCacheDuration 0 defaultSettings)

----- Transformer Aware Applications/Middleware -----
type Middleware m = Application m -> Application m
type Application m = Request -> m Response

--------------- Scotty Applications -----------------
-- Scotty上线文，放在state中
-- 其中包括middleware和路由
data ScottyState e m =
    ScottyState { middlewares :: [Wai.Middleware]
                , routes :: [Middleware m]
                , handler :: ErrorHandler e m
                }
-- 默认的 ScottyState
instance Default (ScottyState e m) where
    def = ScottyState [] [] Nothing
-- 添加中间件
-- 此处的中间件是遵循Wai.Middleware规范的
addMiddleware :: Wai.Middleware -> ScottyState e m -> ScottyState e m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

-- 添加路由
-- 将一个middleware放置在ScottyState的Routes列表中
addRoute :: Middleware m -> ScottyState e m -> ScottyState e m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

-- 第一个参数是handler
-- 第二个参数是ScottyState
addHandler :: ErrorHandler e m -> ScottyState e m -> ScottyState e m
addHandler h s = s { handler = h }
-- runS是ScottyT的域
-- 状态类型是(ScottyState e m)
-- 结果类型是a
newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a }
    deriving ( Functor, Applicative, Monad )


------------------ Scotty Errors --------------------
data ActionError e = Redirect Text
                   | Next
                   | Finish
                   | ActionError e

-- | In order to use a custom exception type (aside from 'Text'), you must
-- define an instance of 'ScottyError' for that type.
class ScottyError e where
    stringError :: String -> e
    showError :: e -> Text

instance ScottyError Text where
    stringError = pack
    showError = id

instance ScottyError e => ScottyError (ActionError e) where
    stringError = ActionError . stringError
    showError (Redirect url)  = url
    showError Next            = pack "Next"
    showError Finish          = pack "Finish"
    showError (ActionError e) = showError e
-- 接收一个异常参数，返回ActionT
type ErrorHandler e m = Maybe (e -> ActionT e m ())

------------------ Scotty Actions -------------------
type Param = (Text, Text)

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq       :: Request
                     , getParams    :: [Param]
                     , getBody      :: IO ByteString
                     , getBodyChunk :: IO BS.ByteString
                     , getFiles     :: [File]
                     }

data RequestBodyState = BodyUntouched
                      | BodyCached ByteString [BS.ByteString] -- whole body, chunks left to stream
                      | BodyCorrupted

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show, Typeable)

instance E.Exception BodyPartiallyStreamed

data Content = ContentBuilder Builder
             | ContentFile    FilePath
             | ContentStream  StreamingBody

data ScottyResponse = SR { srStatus  :: Status
                         , srHeaders :: ResponseHeaders
                         , srContent :: Content
                         }
-- 默认的返回结果
instance Default ScottyResponse where
    def = SR status200 [] (ContentBuilder mempty)

-- ReaderT的域也是个函数runReaderT :: r -> m a
-- 从Reader变量r转化成m a
-- ExceptT (m (Either e a))
-- runExceptT :: ExceptT e m a -> m (Either e a)
-- 当出现异常的时候，会终止执行
newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
    deriving ( Functor, Applicative, MonadIO )

instance (Monad m, ScottyError e) => Monad (ActionT e m) where
    return = ActionT . return
    ActionT m >>= k = ActionT (m >>= runAM . k)
    fail = ActionT . throwError . stringError

instance (Monad m, ScottyError e) => Fail.MonadFail (ActionT e m) where
    fail = ActionT . throwError . stringError

instance ( Monad m, ScottyError e
#if !(MIN_VERSION_base(4,8,0))
         , Functor m
#endif
         ) => Alternative (ActionT e m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, ScottyError e) => MonadPlus (ActionT e m) where
    mzero = ActionT . ExceptT . return $ Left Next
    ActionT m `mplus` ActionT n = ActionT . ExceptT $ do
        a <- runExceptT m
        case a of
            Left  _ -> runExceptT n
            Right r -> return $ Right r
-- 定义了ActionT是如何从底层提升到高层的
-- 从 m 提升到(StateT ScottyResponse m)
-- 再从 (StateT ScottyResponse m) 提升到 (ReaderT ActionEnv (StateT ScottyResponse m))
-- 最后提升到 ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a 
instance MonadTrans (ActionT e) where
    lift = ActionT . lift . lift . lift
-- MonadError 定义
instance (ScottyError e, Monad m) => MonadError (ActionError e) (ActionT e m) where
    throwError = ActionT . throwError
    -- 对ActionT进行catchError，会返回一个全新的ActionT
    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- defH :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionError e -> ActionT e m ()
    -- a = ActionError e , b = ActionT e m ()
    -- catchError :: m a -> (e -> m a) -> m a
    --  ActionT (catchError m (runAM . f))中的catchError为
    -- m `catchE` h = ExceptT $ do
    --  a <- runExceptT m
    --  case a of
    --    Left  l -> runExceptT (h l)
    --    Right r -> return (Right r)
    catchError (ActionT m) f = ActionT (catchError m (runAM . f))


instance (MonadBase b m, ScottyError e) => MonadBase b (ActionT e m) where
    liftBase = liftBaseDefault


instance MonadTransControl (ActionT e) where
     type StT (ActionT e) a = StT (StateT ScottyResponse) (StT (ReaderT ActionEnv) (StT (ExceptT (ActionError e)) a))
     liftWith = \f ->
        ActionT $  liftWith $ \run  ->
                   liftWith $ \run' ->
                   liftWith $ \run'' ->
                   f $ run'' . run' . run . runAM
     restoreT = ActionT . restoreT . restoreT . restoreT

instance (ScottyError e, MonadBaseControl b m) => MonadBaseControl b (ActionT e m) where
    type StM (ActionT e m) a = ComposeSt (ActionT e) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

------------------ Scotty Routes --------------------
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack
