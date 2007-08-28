-----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Utilities for xmonad-utils
--
-----------------------------------------------------------------------------

module Utils where

import Control.Concurrent
import Control.Monad
import Graphics.X11.Xlib
import System.Posix.Types (Fd(..))

-- creates an invisible cursor
nullCursor :: Display -> Window -> IO Cursor
nullCursor d w = do
  let c = Color 0 0 0 0 0
  p <- createPixmap d w 1 1 1
  cursor <- createPixmapCursor d p p c c 0 0
  freePixmap d p
  return cursor

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,_) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

waitASecond :: Int -> IO ()
waitASecond i =
    threadDelay (i*1000000)

-- A version of maskEvent that does not block in foreign calls.
maskEvent' :: Display -> EventMask -> XEventPtr -> IO ()
maskEvent' d m p = do
  pend <- pending d
  if pend /= 0
     then maskEvent d m p
     else do
       threadWaitRead (Fd fd)
       maskEvent' d m p
 where
   fd = connectionNumber d
