{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Main where

import ClassyPrelude

import Control.Arrow
import Control.Arrow.Machine
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import System.Process
import MachineUtils
import qualified Bing
import qualified NatGeo

data BackgroundSource
  = Bing
  | NatGeo
  | Wait
  deriving (Read, Show)

osaScript fileName = unlines [ "tell application \"System Events\""
                             , "\tset picture of every desktop to (\"" <> fileName <> "\" as POSIX file as alias)"
                             , "end tell"
                             ]

restartDock = runCommand $ "killall Dock"

dispatch :: MonadIO m => ProcessA (Kleisli m) (Event BackgroundSource) (Event String)
dispatch = proc a -> do
              cmdEvt <- id -< a
              fin <- stopped -< a
              cmd <- hold Wait -< cmdEvt
              x <- gather -< [cmdEvt, fin]

              resEvt <- case cmd of
                Bing -> edge >>> Bing.getImageUrl -< ()
                NatGeo -> edge >>> NatGeo.getImageUrl -< ()
                Wait -> edge >>> evMap (const "Waiting") -< ()
              res <- hold "Nothing downloaded" -< resEvt
              returnA -< res <$ x
  
instructions :: MonadIO m => m ()
instructions = do
  putStrLn "Bing: download from Bing's picture of the day."
  putStrLn "NatGeo: download from National Geographic's picture of the day."
  putStrLn "end: exits"

parseUrlThrow' mgr url = do
  req <- parseUrlThrow url
  return (req, mgr)

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  runResourceT $ runKleisli (run_ $ machine (const instructions)
                             >>> inputCommand
                             >>> dispatch
                             >>> machine (parseUrlThrow' mgr)
                             >>> downloadHttp "/tmp/bg.jpg"
                             >>> tee
                             >>> machine (const $ lift restartDock)
                            ) (repeat ())
  -- print urls
  -- reqs <- mapM parseUrlThrow urls
  -- runResourceT $ runKleisli (run_ (downloadHttp "/tmp/bg.jpg" >>> tee)) $ zip reqs (repeat mgr)
  -- case urls of
  --   [] -> return ()
  --   _ -> do
  --     _ <- restartDock
  --     return ()
