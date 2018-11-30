{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}

module Main where

import ClassyPrelude

-- import Control.Arrow
-- import Control.Arrow.Machine
-- import Network.HTTP.Conduit
import Control.Monad.Trans.Resource
import System.Process
-- import MachineUtils
import qualified Bing
import qualified NatGeo
import qualified IfaceLift
-- import Network.HTTP.Simple
import Options.Applicative hiding ((<>))
import qualified Options.Applicative as OA
import Options.Applicative.Types
-- import HBORandomMovie
import qualified ProgressEmail.Opts as ProgressEmail

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

-- restartDock :: MonadIO m => ProcessA (Kleisli m) (Event a) (Event ())
-- restartDock = onEnd
--   >>> machine (const $ liftIO $ putStrLn "restartDock onEnd")
--   >>> machine (const $ liftIO restartDock_)
--   >>> machine (liftIO . waitForProcess)
--   >>> machine print

-- restartDock_ :: IO ProcessHandle
-- restartDock_ = runCommand $ "killall Dock"

-- dispatch :: MonadResource m => ProcessA (Kleisli m) (Event BackgroundSource) (Event String)
-- dispatch = proc input -> do
--   cmdEvt <- hold Wait -< input

--   case cmdEvt of
--     Bing -> Bing.getImageUrl -< () <$ input
--     -- NatGeo -> NatGeo.getImageUrl -< () <$ input
--     Iface -> IfaceLift.getImageUrl -< () <$ input
--     Wait -> returnA -< noEvent

instructions :: MonadIO m => m ()
instructions = do
  putStrLn "Bing: download from Bing's picture of the day."
  putStrLn "NatGeo: download from National Geographic's picture of the day."
  putStrLn "Iface: download from InterfaceLift."
  putStrLn "end: exits"

-- parseUrlThrow' :: MonadThrow m => Manager -> String -> m (Request, Manager)
-- parseUrlThrow' mgr url = do
--   req <- setRequestHeaders ifaceRequestHeaders <$> parseUrlThrow url
--   return (req, mgr)

-- wallpaper :: BackgroundSource -> IO ()
-- wallpaper source = do
--   mgr <- newManager tlsManagerSettings
--   runRMachine_ (
--                dispatch
--            >>> machine (parseUrlThrow' mgr)
--            >>> makeRequest
--            >>> sourceHttp_
--            >>> downloadHttp "/tmp/bg.jpg"
--            >>> tee
--            >>> restartDock
--                ) [source]

-- ifaceRequestHeaders = [ ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36")
--                       , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
--                       ]

wallpaperDispatch :: BackgroundSource -> IO ()
wallpaperDispatch NatGeo = NatGeo.download "/tmp/bg.jpg"
wallpaperDispatch Bing = Bing.getImageUrl
wallpaperDispatch Iface = IfaceLift.getImageUrl

wallpaperParser :: Parser (IO ())
wallpaperParser = wallpaperDispatch <$> (
  flag' Bing
    (  long "bing"
    <> short 'b'
    <> help "Download from Bing's picture of the day."
    )
  <|> flag' NatGeo
    ( long "natgeo"
      <> short 'n'
      <> help "Download from National Geographic's picture of the day."
    )
  <|> flag' Iface
    ( long "interfacelift"
      <> short 'i'
      <> help "Download a random background from InterfaceLift."
    )
  )

wallpaperInfo :: ParserInfo (IO ())
wallpaperInfo = info (helper <*> wallpaperParser)
  ( fullDesc
  <> progDesc "Download an image to use as a desktop background from one of the supported sources."
  )

parseCommands :: Parser (IO ())
parseCommands = subparser
  (  command "backgrounds" wallpaperInfo
  <> command "progress-email" ProgressEmail.parserInfo
--  <> command "random-movie" movieInfo
  )

commandsInfo :: ParserInfo (IO ())
commandsInfo = info (helper <*> parseCommands)
  ( fullDesc
  <> progDesc "This is a collection of various tools that I use at home for random things."
  )

main :: IO ()
main = join $ execParser commandsInfo

  -- print urls
  -- reqs <- mapM parseUrlThrow urls
  -- runResourceT $ runKleisli (run_ (downloadHttp "/tmp/bg.jpg" >>> tee)) $ zip reqs (repeat mgr)
  -- case urls of
  --   [] -> return ()
  --   _ -> do
  --     _ <- restartDock
  --     return ()
