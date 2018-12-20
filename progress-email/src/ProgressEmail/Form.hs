{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProgressEmail.Form where

import ClassyPrelude
import Text.Digestive
import ProgressEmail.Types
import Text.Digestive.Lucid.Html5
import Lucid

ticketForm :: Monad m => Form Text m Ticket
ticketForm = Ticket
  <$> "EPC-" .: validate (readText "Please only enter a number") (text Nothing)
  <*> fmap fromStrict ("Description" .: text Nothing)

postTicketForm :: Monad m => Text -> Text -> Env m
postTicketForm ticket desc _ = pure [TextInput ticket, TextInput desc]

ticketHtml :: View Text -> Html ()
ticketHtml v = do
  form_ [action_ "ticket"] $ do
    label "EPC-" v "EPC-"
    inputText "EPC-" v
    label "Description" v "Description"
    inputText "Description" v
    input_ [type_ "submit", value_ "Submit", method_ "POST"]

readText :: (Read a, Monad m) => String -> Text -> m a
readText failMessage = maybe (fail failMessage) pure . readMay
