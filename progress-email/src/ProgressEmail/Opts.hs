{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProgressEmail.Opts where

import ClassyPrelude
import Options.Applicative
import ProgressEmail
import ProgressEmail.Server (startServer)

parserInfo :: ParserInfo (IO ())
parserInfo = info (helper <*> parser)
  (  fullDesc
  <> progDesc "A collection of utilities to aid in tracking progress emails and tickets."
  )

parser :: Parser (IO ())
parser = subparser
  (  command "simple-render" renderParserInfo
  <> command "web-server" serverParserInfo
  )

renderParserInfo :: ParserInfo (IO ())
renderParserInfo = info (helper <*> renderParser)
  (  fullDesc
  <> progDesc "A small utility to aid in generating my progress emails. Mostly done for fun."
  )

renderParser :: Parser (IO ())
renderParser = buildAndWriteEmail
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

serverParserInfo :: ParserInfo (IO ())
serverParserInfo = info (helper <*> serverParser)
  (fullDesc
  <> progDesc "Launches the web server for handling progress email related stuff."
  )

serverParser :: Parser (IO ())
serverParser = startServer
  <$> option auto
  (  short 'p'
  <> long "port"
  <> help "The port number to listen for requests on."
  )
  <*> strOption
  (  short 'd'
  <> long "dbPath"
  <> help "The path to the database."
  )
