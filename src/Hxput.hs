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

import Data.Char

main :: IO ()
main = do 
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 200 100 0 0 0
  p <- internAtom dpy "PRIMARY" True
  ty <- internAtom dpy "UTF8_STRING" False
  xSetSelectionOwner dpy p win currentTime
  winOwn <- xGetSelectionOwner dpy p
  print win
  print ty
  if winOwn == win
    then putStrLn "Yes"
    else putStrLn "No"
  allocaXEvent $ \e -> do
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
                                         (map (fromIntegral . ord) $ "Boo")
                         setSelectionNotify replyPtr (ev_requestor ev) (ev_selection ev) (ev_target ev) (ev_property ev) (ev_time ev)
                         sendEvent dpy (ev_requestor ev) False noEventMask replyPtr
               sync dpy False
               putStrLn "Sent"
       else do putStrLn "Failed!"
  destroyWindow dpy win
  exitWith ExitSuccess
