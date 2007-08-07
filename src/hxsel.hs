module Main where
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitWith, ExitCode(..))

import Data.Maybe
import Data.Char


main :: IO ()
main = do 
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 200 100 0 0 0
  p <- internAtom dpy "PRIMARY" True
  clp <- internAtom dpy "BLITZ_SEL_STRING" False
  xConvertSelection dpy p sTRING clp win currentTime
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    if ev_event_type ev == selectionNotify 
       then do res <- getWindowProperty8 dpy clp win
               putStrLn $ map (chr . fromIntegral)  . fromMaybe [] $ res
       else do putStrLn "Failed!"
  destroyWindow dpy win
  exitWith ExitSuccess

