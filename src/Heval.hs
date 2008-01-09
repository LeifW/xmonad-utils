-----------------------------------------------------------------------------
-- |
-- Module      :  Heval
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
--  A simple evaluator used by a XMonad prompt
--
-----------------------------------------------------------------------------

module Main where

import GHC
import Panic
import DynFlags
import PackageConfig
import ErrUtils
import SrcLoc  (SrcSpan)
import Outputable

import Control.Concurrent
import Control.Exception
import Data.Char
import Data.Dynamic
import Data.List
import Data.Maybe
import Prelude hiding (catch)
import System.Exit
import System.IO
import System.Random
import System.Posix.Resource
import System.Posix.Signals

ghcPath :: String
ghcPath = "/usr/lib/ghc-6.8.2"

myLog :: Severity -> SrcSpan -> PprStyle -> Message -> IO ()
myLog = \severity _ style msg ->
             case severity of
               SevInfo  -> hPutStrLn stdout (show (msg style))
               SevFatal -> hPutStrLn stdout (show (msg style))
               _        -> hPutStrLn stdout (show (msg style))

rlimit :: ResourceLimit
rlimit = ResourceLimit 5

main :: IO ()
main = do
  setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit)
  defaultErrorHandler defaultDynFlags $ do
         s <- getLine
         let exps = read s :: [String]
         ses <- initSession
         case exps of
           []     -> return ()
           (x:[]) -> runExp ses x
           xs      -> do updateSession ses (init xs)
                         runExp ses (last xs)

initSession :: IO Session
initSession = do
  ses <- newSession (Just ghcPath)
  df <- getSessionDynFlags ses
  setSessionDynFlags ses df  {log_action = myLog }
  setContext ses [] [mkModule (stringToPackageId "base") (mkModuleName "Prelude")]
  return ses

updateSession :: Session ->  [String] ->  IO ()
updateSession ses l =
    mapM_ (runWithTimeOut 3 . flip (runStmt ses) SingleStep) l

exprToRun :: String -> IO String
exprToRun expr = do
  x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
  return ("let { "++ x ++
          " = " ++ expr ++
          "\n} in take 2048 (show " ++ x ++
          ")")

runWithTimeOut :: Int -> IO a -> IO a
runWithTimeOut to action = do
  t <- forkIO $ checkerThread
  res <- action
  killThread t
  return res
  where
    -- if this thread is not killed within t seconds it will raise the
    -- equivalent of a ^C
    checkerThread = do
      threadDelay (to * 1000000)
      -- FIXME
      hPutStrLn stdout "Interrupted."
      raiseSignal sigQUIT

runExp :: Session -> String -> IO ()
runExp ses (':':com) =
    specialCommand ses com
runExp ses s
    -- nothing: restart
    | s == "" || s == "\r" || s == "\n" = do
  return ()
     -- let: bind and update session
    | "let " `isPrefixOf` s = do
  runWithTimeOut 3 $ runStmt ses s SingleStep
  return ()
    -- something to eval
    | otherwise = do
  expr <- exprToRun s
  catch (go expr) (\e -> hPutStrLn stdout $ showException e)
    where
      go e = do
        str <- evalExpr ses e
        putStrLn str

evalExpr :: Session -> String -> IO String
evalExpr ses expr = do
  maybe_dyn <- dynCompileExpr ses expr
  case maybe_dyn of
    Just dyn -> return $ (fromMaybe "" (fromDynamic dyn :: Maybe String))
    _ -> do return []

-- special commands
specialCommand :: Session -> String -> IO ()
specialCommand _ com
    -- stop: stop server
    | "stop" `isPrefixOf` com = exitWith ExitSuccess
    -- quit: exit
    | "quit" `isPrefixOf` com = return ()
    | otherwise = return ()

