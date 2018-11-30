{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProgressEmail.Opts where

import ClassyPrelude
import Options.Applicative
import ProgressEmail

parserInfo :: ParserInfo (IO ())
parserInfo = info (helper <*> parser)
  (  fullDesc
  <> progDesc "A small utility to aid in generating my progress emails. Mostly done for fun."
  )

parser :: Parser (IO ())
parser = buildAndWriteEmail
  <$> strOption
  (  short 'i'
  <> long "input"
  <> help "The path to the progress file."
  )
  <*> strOption
  (  short 'o'
  <> long "output"
  <> help "The path where the resulting markdown should be written."
  )
