{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ProgressEmail.Html where

import ClassyPrelude
import Html
import Html.Attribute
import Data.Proxy
import ProgressEmail
import Data.Time
import qualified Data.Text.Lazy as TL
import ProgressEmail.Types
import ProgressEmail.Tupleable
import Control.Lens ((^.), view, to)

-- header = head_ $ link_A (rel_ (Proxy :: Proxy "stylesheet") # href_ (Proxy :: Proxy "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"))
header = head_ $ (link_A (rel_ (Proxy :: Proxy "stylesheet") # href_ (Proxy :: Proxy "http://127.0.0.1:3000/style.css")))
               # (link_A (rel_ (Proxy :: Proxy "stylesheet") # href_ (Proxy :: Proxy "https://unpkg.com/purecss@1.0.0/build/base-min.css")))

-- renderTickets = fmap renderTicket

renderTicket ticket = tr_ $ td_ ("EPC-" <> tshow (number ^. ticketNumberInt))
                          # td_ description
                          # td_ (scriptStatus ticket)
  where
    number = _ticketNumber ticket
    description = _ticketDescription ticket
    scriptStatus = view $ ticketScriptedStatus . to tshow

renderMessage (Message body tickets) =
    p_ body
  # fmap renderTicket tickets

renderExecutionSummary progress = table
  [ ["Executed", tshow total]
  , ["Passed"  , tshow (passed progress)]
  , ["Failed"  , tshow (failed progress)]
  ]
  where
    total = failed progress + passed progress

renderProgress progress =
  ul_ (fmap li_
    [ "Overall test execution progress is " <> dispPercentage (percentage progress) <> " complete."
    , "Total test scenarios executed today: " <> dispInt (executedToday progress) <> "\n"
    , "Tests in progress: " <> dispInt (inProgress progress) <> "\n"
    ])
  # renderExecutionSummary progress
  # message progress

renderTicketTable = table_A (class_ (Proxy :: Proxy "pure-table pure-table-horizontal pure-table-noborder")) . tbody_ . fmap renderTicket . asList
  where
    asList :: [Ticket a] -> [Ticket a]
    asList = id

table :: [[Text]] -> (:@:)
  'Table
  ('ClassA := Proxy "pure-table pure-table-horizontal pure-table-noborder")
  ('Tbody > ['Tr > ['Td > Text]])
table body = table_A (class_ (Proxy :: Proxy "pure-table pure-table-horizontal pure-table-noborder")) (tbody_ $ fmap (tr_ . fmap td_) body)

renderMarkdown currentTime (MarkDown progress title) =
    h1_ (date currentTime <> " " <> title)
  # renderProgress progress

buildHtml :: FilePath -> IO (Either String Email)
buildHtml fp = do
  currentTime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  eMd <- loadMarkDown fp
  flip traverse eMd $ \md -> do
    email <- loadMessage md
    let body = body_ $ renderMarkdown currentTime email
    pure . Email . renderText $ html_ (header # body)

buildAndWriteHtml :: FilePath -> FilePath -> IO ()
buildAndWriteHtml source dest = do
  eEmail <- buildHtml source
  case eEmail of
    Left err -> putStrLn $ pack err
    Right email -> writeEmail dest email
