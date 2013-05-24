-----------------------------------------------------------------------------
-- |
-- Module      :  Hhp
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Hides the pointer after some inactivity
--
-----------------------------------------------------------------------------

module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit

import Utils

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  waitForMotion dpy rootw

-- hides and grabs the pointer till the user moves it
hidePointer :: Display -> Window -> IO ()
hidePointer d w = do
  let em = buttonPressMask .|. pointerMotionMask
  cursor <- nullCursor d w
  ps <- grabPointer d w False em grabModeAsync
                    grabModeAsync w cursor currentTime
  when (ps /= grabSuccess) $ do
        waitASecond 1
        hidePointer d w
  allocaXEvent $ \e -> do
      maskEvent d em e
      ungrabPointer d currentTime
      waitForMotion d w

-- when the pointer is not moved a timer starts: after ten seconds, if
-- no motion interrupts the timer, the pointer is grabbed and made
-- invisible.
waitForMotion :: Display -> Window -> IO ()
waitForMotion d w = do
  mt <- myThreadId
  t <- forkIO (timer mt)
  block $ go t
    where
      -- interrupt the waiting for motion (and thus hide the pointer)
      timer t = do
        waitASecond 10
        throwTo t (ErrorCall "done")
      -- wait for the next motion, and restart the timer (?)
      stopAndWait t = do
          allocaXEvent $ maskEvent' d pointerMotionMask
          -- this seems to just suspend the timer...
          throwTo t (ExitSuccess)
          waitForMotion d w
      -- wait for a timer interrupt to hide the pointer
      go t = do
        catch (unblock $ stopAndWait t) (const $ hidePointer d w :: ErrorCall -> IO ())
