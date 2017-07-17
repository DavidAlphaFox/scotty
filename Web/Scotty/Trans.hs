{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- The functions in this module allow an arbitrary monad to be embedded
-- in Scotty's monad transformer stack in order that Scotty be combined
-- with other DSLs.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
module Web.Scotty.Trans
    ( -- * scotty-to-WAI
      scottyT, scottyAppT, scottyOptsT, scottySocketT, Options(..)
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, header, headers, body, bodyReader, param, params, jsonData, files
      -- ** Modifying the Response and Redirecting
    , status, addHeader, setHeader, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json, stream, raw
      -- ** Exceptions
    , raise, rescue, next, finish, defaultHandler, ScottyError(..), liftAndCatchIO
      -- * Parsing Parameters
    , Param, Parsable(..), readEither
      -- * Types
    , RoutePattern, File
      -- * Monad Transformers
    , ScottyT, ActionT
    ) where

import Blaze.ByteString.Builder (fromByteString)

import Control.Monad (when)
import Control.Monad.State (execState, modify)
import Control.Monad.IO.Class

import Data.Default.Class (def)

import Network (Socket)
import Network.HTTP.Types (status404, status500)
import Network.Wai
import Network.Wai.Handler.Warp (Port, runSettings, runSettingsSocket, setPort, getPort)

import Web.Scotty.Action
import Web.Scotty.Route
import Web.Scotty.Internal.Types hiding (Application, Middleware)
import Web.Scotty.Util (socketDescription)
import qualified Web.Scotty.Internal.Types as Scotty

-- | Run a scotty application using the warp server.
-- NB: scotty p === scottyT p id
scottyT :: (Monad m, MonadIO n)
        => Port
        -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
        -> ScottyT e m ()
        -> n ()
scottyT p = scottyOptsT $ def { settings = setPort p (settings def) }

-- | Run a scotty application using the warp server, passing extra options.
-- NB: scottyOpts opts === scottyOptsT opts id
scottyOptsT :: (Monad m, MonadIO n)
            => Options
            -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
            -> ScottyT e m ()
            -> n ()
scottyOptsT opts runActionToIO s = do
    when (verbose opts > 0) $
        liftIO $ putStrLn $ "Setting phasers to stun... (port " ++ show (getPort (settings opts)) ++ ") (ctrl-c to quit)"
    -- (=<<) :: Monad m => (a -> m b) -> m a -> m b
    -- runSettings :: Settings -> Application -> IO ()
    liftIO . runSettings (settings opts) =<< scottyAppT runActionToIO s

-- | Run a scotty application using the warp server, passing extra options, and
-- listening on the provided socket.
-- NB: scottySocket opts sock === scottySocketT opts sock id
scottySocketT :: (Monad m, MonadIO n)
              => Options
              -> Socket
              -> (m Response -> IO Response)
              -> ScottyT e m ()
              -> n ()
scottySocketT opts sock runActionToIO s = do
    when (verbose opts > 0) $ do
        d <- liftIO $ socketDescription sock
        liftIO $ putStrLn $ "Setting phasers to stun... (" ++ d ++ ") (ctrl-c to quit)"
    liftIO . runSettingsSocket (settings opts) sock =<< scottyAppT runActionToIO s

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
-- NB: scottyApp === scottyAppT id
scottyAppT :: (Monad m, Monad n)
           => (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
           -> ScottyT e m ()
           -> n Application
scottyAppT runActionToIO defs = do
    -- Evaluate a state computation with the given initial state
    -- and return the final state, discarding the final value.
    -- def是Data.Default.Class的函数，根据类型推导会自动反射为相应的instance的def
    -- 从默认状态构建应用状态
    let s = execState (runS defs) def
    -- s是ScottyState,其中包含了所有的路由，中间件和异常处理的handler
    -- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
    -- (flip ($)) :: a1 -> (a1 -> c) -> c
    -- 那么routes的类型应该是 t (a -> c) 一列表的函数
    -- notFoundApp 是初始值
    -- 在foldl中 b = a1， a = a1 -> c
    -- (flip ($)) notFoundApp (routes s) req) ::
    -- (a1 -> (a1 -> c) -> c) -> a1 -> Middleware m -> 
    let rapp req callback = runActionToIO (foldl (flip ($)) notFoundApp (routes s) req) >>= callback
    -- foldl :: (b -> a -> b) -> b -> t a -> b
    return $ foldl (flip ($)) rapp (middlewares s)

notFoundApp :: Monad m => Scotty.Application m
-- type Application m ::  Request -> m Response
-- 此处忽略req
notFoundApp _ = return $ responseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Global handler for uncaught exceptions.
--
-- Uncaught exceptions normally become 500 responses.
-- You can use this to selectively override that behavior.
--
-- Note: IO exceptions are lifted into 'ScottyError's by 'stringError'.
-- This has security implications, so you probably want to provide your
-- own defaultHandler in production which does not send out the error
-- strings as 500 responses.
defaultHandler :: (ScottyError e, Monad m) => (e -> ActionT e m ()) -> ScottyT e m ()
-- 默认的handler，第一个参数是异常处理函数
-- 最终生成ScottyT
-- modify :: MonadState s m => (s -> s) -> m ()
-- 先构建ErrorHandler
-- 此时返回的是一个ScottyT结构，实际上是封装了一个待执行的函数
-- modify 需要执行上下文
defaultHandler f = ScottyT $ modify $ addHandler $ Just (\e -> status status500 >> f e)

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyT e m ()
-- 添加中间件
middleware = ScottyT . modify . addMiddleware
