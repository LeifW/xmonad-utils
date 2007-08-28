-----------------------------------------------------------------------------
-- |
-- Module      :  hmanage
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Toggle override_redirect for an X window
--
-----------------------------------------------------------------------------
module Main where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Environment

usage :: String -> String
usage n = "Usage: " ++ n ++ " manage/unmanage windowID"

main :: IO ()
main = do
  args <- getArgs
  pn <- getProgName
  let (win,ac) = case args of
                   [] -> error $ usage pn
                   w -> case (w !!0) of 
                          "manage" -> (window, False)
                          "unmanage" ->  (window, True)
                          _ -> error $ usage pn
                       where window = case  (w !! 1) of 
                                               [] -> error $ usage pn
                                               x -> read x :: Window
  dpy <- openDisplay ""
  unmapWindow dpy win
  sync dpy False
  allocaSetWindowAttributes $
       \attributes -> do
         set_override_redirect attributes ac
         changeWindowAttributes dpy win cWOverrideRedirect attributes
  mapWindow dpy win
  sync dpy False

