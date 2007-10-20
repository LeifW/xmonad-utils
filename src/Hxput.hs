-----------------------------------------------------------------------------
-- |
-- Module      :  hxput
-- Copyright   :  (c) Andrea Rossato, Matthew Sackman
-- License     :  BSD3
--
-- Maintainer  :  Matthew Sackman <matthew@wellquite.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Sets the mouse selection
--
-----------------------------------------------------------------------------

module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitWith, ExitCode(..))
import System.Environment

import Data.Char

main :: IO ()
main = do
  (text:_) <- getArgs
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  p <- internAtom dpy "PRIMARY" True
  ty <- internAtom dpy "UTF8_STRING" False
  xSetSelectionOwner dpy p win currentTime
  winOwn <- xGetSelectionOwner dpy p
  if winOwn == win
    then do allocaXEvent $ processEvent dpy ty text
            destroyWindow dpy win
            exitWith ExitSuccess
    else do putStrLn "Unable to obtain ownership of the selection"
            destroyWindow dpy win
            exitWith (ExitFailure 1)

processEvent :: Display -> Atom -> [Char] -> XEventPtr -> IO a
processEvent dpy ty text e = do
  nextEvent dpy e
  ev <- getEvent e
  if ev_event_type ev == selectionRequest
    then do print ev
            -- selection == eg PRIMARY
            -- target == type eg UTF8
            -- property == property name or None
            allocaXEvent $ \replyPtr -> do
                   changeProperty8 (ev_event_display ev)
                                   (ev_requestor ev)
                                   (ev_property ev)
                                   ty
                                   propModeReplace
                                   (map (fromIntegral . ord) text)
                   setSelectionNotify replyPtr (ev_requestor ev) (ev_selection ev) (ev_target ev) (ev_property ev) (ev_time ev)
                   sendEvent dpy (ev_requestor ev) False noEventMask replyPtr
            sync dpy False
            putStrLn "Sent"
    else do putStrLn "Unexpected Message Received"
            print ev
  processEvent dpy ty text e
