-- |
-- Module      :  System.Console.Questioner.ProgressIndicators
-- Description :  Provides progress indicators and spinners
-- Copyright   :  (c) Pedro Yamada
-- License     :  MIT
--
-- Maintainer  :  Pedro Yamada <tacla.yamada@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (not tested on multiple environments)
--
-- Shamefully steals ideas from modules like `Inquirer.js` and `go-spin`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Console.Spinners
  where

import ClassyPrelude
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import System.Console.ANSI (clearLine, setCursorColumn)
import Control.Concurrent (threadDelay)

data SpinnerConfig = SpinnerConfig
  { _spinnerTheme :: SpinnerTheme
  , _spinnerColor :: SpinnerColor
  , _spinnerInterval :: Int
  }

type SpinnerTheme = [Char]
type SpinnerColor = Text -> Text

addSpinner :: SpinnerConfig -> Text -> IO a -> IO (Either () a)
addSpinner config prompt act = do
  hSetBuffering stdout NoBuffering
  result <- race (displaySpinner config prompt) act
  putStrLn ""
  hSetBuffering stdout LineBuffering
  pure result

displaySpinner :: SpinnerConfig -> Text -> IO ()
displaySpinner (SpinnerConfig theme colorFn interval) prompt =
  mapM_ (spinnerStep interval colorFn prompt) theme

spinnerStep :: Int -> (Text -> Text) -> Text -> Char -> IO ()
spinnerStep interval colorFn prompt char = do
  clearLine
  setCursorColumn 0
  putStr $ " " <> colorFn (pack $ pure char) <> " " <> prompt
  threadDelay interval

dots1SpinnerTheme :: [Char]
dots1SpinnerTheme = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']

dots1Spinner :: SpinnerConfig
dots1Spinner = SpinnerConfig (concat $ repeat dots1SpinnerTheme) id (1000 * 80)

addDots1Spinner :: Text -> IO a -> IO (Either () a)
addDots1Spinner = addSpinner dots1Spinner

spinner :: Text -> IO a -> IO a
spinner prompt act = do
  eResult <- addDots1Spinner prompt act
  case eResult of
    Left () -> error "The spinner action exited before the action!"
    Right a -> pure a
