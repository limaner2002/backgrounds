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
import qualified IfaceLift
import Network.HTTP.Simple

data BackgroundSource
  = Bing
  | NatGeo
  | Iface
  | Wait
  deriving (Read, Show)

osaScript :: (Textual t, Semigroup t) => t -> t
osaScript fileName = unlines [ "tell application \"System Events\""
                             , "\tset picture of every desktop to (\"" <> fileName <> "\" as POSIX file as alias)"
                             , "end tell"
                             ]

restartDock :: MonadIO m => ProcessA (Kleisli m) (Event a) (Event ())
restartDock = onEnd
  >>> machine (const $ liftIO $ putStrLn "restartDock onEnd")
  >>> machine (const $ liftIO restartDock_)
  >>> machine (liftIO . waitForProcess)
  >>> machine print

restartDock_ :: IO ProcessHandle
restartDock_ = runCommand $ "killall Dock"

dispatch :: MonadResource m => ProcessA (Kleisli m) (Event BackgroundSource) (Event String)
dispatch = proc input -> do
  cmdEvt <- hold Wait -< input

  case cmdEvt of
    Bing -> Bing.getImageUrl -< () <$ input
    NatGeo -> NatGeo.getImageUrl -< () <$ input
    Iface -> IfaceLift.getImageUrl -< () <$ input
    Wait -> returnA -< noEvent

instructions :: MonadIO m => m ()
instructions = do
  putStrLn "Bing: download from Bing's picture of the day."
  putStrLn "NatGeo: download from National Geographic's picture of the day."
  putStrLn "Iface: download from InterfaceLift."
  putStrLn "end: exits"

parseUrlThrow' :: MonadThrow m => Manager -> String -> m (Request, Manager)
parseUrlThrow' mgr url = do
  req <- setRequestHeaders ifaceRequestHeaders <$> parseUrlThrow url
  return (req, mgr)

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  runRMachine_ (machine (const instructions)
           >>> inputCommand'
           >>> dispatch
           >>> machine (parseUrlThrow' mgr)
           >>> makeRequest
           >>> sourceHttp_
           >>> downloadHttp "/tmp/bg.jpg"
           >>> tee
           >>> restartDock
               ) [()]

ifaceRequestHeaders = [ ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
                      , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                      ]

  -- print urls
  -- reqs <- mapM parseUrlThrow urls
  -- runResourceT $ runKleisli (run_ (downloadHttp "/tmp/bg.jpg" >>> tee)) $ zip reqs (repeat mgr)
  -- case urls of
  --   [] -> return ()
  --   _ -> do
  --     _ <- restartDock
  --     return ()
