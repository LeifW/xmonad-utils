-----------------------------------------------------------------------------
-- |
-- Module      :  hslock
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A simple screen locker in Haskell
--
-- Works only with shadow passords and if set suid root
--
-- Compile with:
--
-- hsc2hs hslock.hsc
-- ghc --make hslock.hs -fglasgow-exts -lcrypt
--
-- Then, as root, set it suid root:
-- chmod u+s /path/to/hslock
-----------------------------------------------------------------------------

module Main where

import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Foreign.C
import Foreign
import System.Environment

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Utils

data Spwd = 
    Spwd { sp_namp :: CString
         , sp_pwdp :: CString
         }

#include "shadow.h"
#define _XOPEN_SOURCE
#include "unistd.h"

foreign import ccall unsafe "shodow.h getspnam"
    getspan :: CString -> IO (Ptr Spwd)

instance Storable Spwd where
    sizeOf    _ = #{size struct spwd}
    alignment _ = alignment (undefined :: CInt)
    peek p = Spwd `fmap` #{peek struct spwd, sp_namp} p
                  `ap`   #{peek struct spwd, sp_pwdp} p
    poke p (Spwd n pw) = do
        #{poke struct spwd, sp_namp} p n
        #{poke struct spwd, sp_pwdp} p pw

getpass :: String -> IO Spwd
getpass name = 
  withCString name $ \ c_name -> do
    s <- throwIfNull "No user entry" $ getspan c_name
    peek s

foreign import ccall unsafe "unistd.h crypt"
  hcrypt :: CString -> CString -> IO CString

encrypt_pass :: String -> String -> IO String
encrypt_pass key salt = do
  withCString key $ \k -> 
      withCString salt $ \s -> do
          e <- hcrypt k s
          peekCString e

verifyPWD :: String -> String -> IO Bool
verifyPWD name pass = do
  u <- getpass name
  pw <- peekCString (sp_pwdp u)
  e <- encrypt_pass pass pw
  return (pw == e)

main :: IO ()
main = do
  s <- newIORef []
  d <- catch (getEnv "DISPLAY") ( const $ return [])
  dpy <- openDisplay d
  let dflt = defaultScreen dpy
      scr  = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt
  win <- mkUnmanagedWindow dpy scr rootw 0 0 (widthOfScreen scr) (heightOfScreen scr)
  selectInput dpy win keyPress
  mapWindow dpy win
  sync dpy False
  i <- grabInput dpy win
  if i == grabSuccess 
     then do
       eventLoop dpy s
       ungrabKeyboard dpy currentTime
       ungrabPointer dpy currentTime
     else putStrLn "Cannot grab the keyboard!"
  destroyWindow dpy win
  sync dpy False

grabInput :: Display -> Window -> IO GrabStatus
grabInput dpy win = do
  cursor <- nullCursor dpy win
  grabPointer dpy win False noEventMask grabModeAsync grabModeAsync win cursor currentTime
  ks <- grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
  if (ks /= grabSuccess) 
      then do
        threadDelay (1*1000000)
        grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
      else return ks

eventLoop :: Display -> IORef String -> IO ()
eventLoop d i = do
  (keysym,string,event) <- 
      allocaXEvent $ \e -> do 
          maskEvent d keyPressMask e
          ev <- getEvent e
          (ks,s) <- if ev_event_type ev == keyPress
                    then lookupString $ asKeyEvent e
                    else return (Nothing, "")
          return (ks,s,ev)
  handle d i (fromMaybe xK_VoidSymbol keysym,string) event

type KeyStroke = (KeySym, String)

handle :: Display -> IORef String -> KeyStroke -> Event -> IO ()
handle d i (ks,str) (KeyEvent {ev_event_type = t}) 
-- Return: check password
    | t == keyPress && ks == xK_Return = do
  u <- getEnv "USER"
  p <- readIORef i
  b <- verifyPWD u p
  if b then return ()
       else modifyIORef i (\_ -> []) >> eventLoop d i
-- Escape: restart
    | t == keyPress && ks == xK_Escape = do
  modifyIORef i (\_ -> [])
  eventLoop d i
-- empty string -> loop
    | t == keyPress && str == "" = eventLoop d i
-- something to save
    | otherwise = do
  modifyIORef i (\s -> s ++ str)
  eventLoop d i
handle d i _ _ = eventLoop d i

mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect .|. cWBackPixel
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           set_background_pixel attributes $ blackPixel dpy (defaultScreen dpy)
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
                        inputOutput visual attrmask attributes
