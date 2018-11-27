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
{-# LANGUAGE TemplateHaskell #-}

module System.Console.Spinners
  where

import ClassyPrelude
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import System.Console.ANSI (clearLine, setCursorColumn)
import Control.Concurrent (threadDelay)
import Control.Lens

data SpinnerConfig = SpinnerConfig
  { _spinnerTheme :: SpinnerLoop -- | The animation loop of the spinner
  , _spinnerColor :: SpinnerColor -- | A function to change the color of the spinner
  , _spinnerInterval :: Int       -- | The delay between steps in the spinner animation
  }

-- | Wrapper for an infinite loop for the spinner animation. The smart
-- constructor 'mkSpinnerLoop' should be used to create a
-- 'SpinnerLoop'
newtype SpinnerLoop = UnsafeSpinnerLoop
  { _spinnerLoop :: [Char]
  }

-- | A function to change the color of the spinner
type SpinnerColor = Text -> Text

makeLenses ''SpinnerConfig
makeLenses ''SpinnerLoop

  -- | Creates an infinite 'SpinnerLoop' from a list of characters
  -- ensuring that the spinner animation does not terminate before the
  -- action does.
mkSpinnerLoop :: [Char] -> SpinnerLoop
mkSpinnerLoop = UnsafeSpinnerLoop . concat . repeat

addSpinner :: SpinnerConfig -> Text -> IO a -> IO (Either () a)
addSpinner config prompt act = do
  hSetBuffering stdout NoBuffering
  result <- race (displaySpinner config prompt) act
  putStrLn ""
  hSetBuffering stdout LineBuffering
  pure result

displaySpinner :: SpinnerConfig -> Text -> IO ()
displaySpinner (SpinnerConfig loop colorFn interval) prompt =
  mapM_ (spinnerStep interval colorFn prompt) $ loop ^. spinnerLoop

spinnerStep :: Int -> (Text -> Text) -> Text -> Char -> IO ()
spinnerStep interval colorFn prompt char = do
  clearLine
  setCursorColumn 0
  putStr $ " " <> colorFn (pack $ pure char) <> " " <> prompt
  threadDelay interval

dots1SpinnerTheme :: [Char]
dots1SpinnerTheme = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']

dots1Spinner :: SpinnerConfig
dots1Spinner = SpinnerConfig (mkSpinnerLoop dots1SpinnerTheme) id (1000 * 80)

addDots1Spinner :: Text -> IO a -> IO (Either () a)
addDots1Spinner = addSpinner dots1Spinner

withSpinner :: Text -> IO a -> IO a
withSpinner prompt act = do
  eResult <- addDots1Spinner prompt act
  case eResult of
    Left () -> error "The spinner action exited before the action!"
    Right a -> pure a
