{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProgressEmail.Form where

import ClassyPrelude
import Text.Digestive
import ProgressEmail.Types
import Text.Digestive.Lucid.Html5
import Lucid
import qualified Web.FormUrlEncoded as WF

ticketForm :: Monad m => [Ticket [Int]] -> Form (Html ()) m (Ticket [Int])
ticketForm tickets = Ticket
  <$> "ticketNum" .: validate (numValidation tickets) (text Nothing)
  <*> fmap fromStrict ("description" .: (validate checkDescription $ text Nothing))
  <*> "relations" .: choiceMultiple' ticketChoices Nothing
  <*> fmap (fmap fromStrict) ("assignedUser" .: optionalText Nothing)
  <*> fmap TicketStatus ("ticketStatus" .: (validate checkRequired $ text Nothing))
  where
    checkDescription = checkRequired >=> limitDescription
    ticketNums = fmap ticketNumber tickets
    ticketChoice :: Int -> (Int, Html ())
    ticketChoice i = (i, toHtml $ tshow i)
    ticketChoices :: [(Int, Html ())]
    ticketChoices = fmap ticketChoice ticketNums

postTicketForm :: (Monad m, MonadIO m) => WF.Form -> Env m
postTicketForm form paths = do
  let key = intercalate "." paths
      x = WF.lookupUnique key form
  case x of
    Left _ -> pure mempty
    Right y -> pure [TextInput y]

ticketHtml :: View (Html ()) -> Html ()
ticketHtml v = do
  form_ [action_ "new", method_ "POST"] $ do
    inputText' "ticketNum" "Ticket Number" v
    inputParagraph' "description" "Description" v

    label "relations" v "Related Tickets"
    inputSelect "relations" v

    inputText' "assignedUser" "Assigned User" v
    inputText' "ticketStatus" "Status" v

    inputSubmit "Submit"

inputText' :: Text -> Text -> View (Html ()) -> Html ()
inputText' varName labelName v =
  case errs of
    Nothing ->
      div_ [class_ "input-group"] $ do
        label varName v $ toHtml labelName
        inputText varName v
    Just errMsg ->
      div_ [class_ "input-group error"] $ do
        label varName v $ toHtml labelName
        inputText varName v
        div_ [class_ "error-message"] $ toHtml errMsg
  where
    errs = checkErrors varName v

inputParagraph' :: Text -> Text -> View (Html ()) -> Html ()
inputParagraph' varName labelName v =
  case errs of
    Nothing ->
      div_ [class_ "input-group"] $ do
        label varName v $ toHtml labelName
        inputParagraph varName v
    Just errMsg ->
      div_ [class_ "input-group error"] $ do
        label varName v $ toHtml labelName
        inputParagraph varName v
        div_ [class_ "error-message"] $ toHtml errMsg
  where
    errs = checkErrors varName v

inputParagraph :: Text -> View (Html ()) -> Html ()
inputParagraph varName v =
  textarea_ [ rows_ "10"
            , cols_ "30"
            , name_ varName'
            , id_ varName'
            , value_ input
            ]
    $ toHtml input
  where
    varName' = absoluteRef varName v
    input = fieldInputText varName v

checkErrors :: Text -> View a -> Maybe a
checkErrors k = lookup k . fmap (\(p, v) -> (intercalate "." p, v)) . viewErrors
                -- Validations
readText :: Read a => Text -> Text -> Result (Html ()) a
readText failMessage = maybe (Error $ toHtml failMessage) Success . readMay

limitDescription :: Text -> Result (Html ()) Text
limitDescription desc
  | length desc <= 256 = Success desc
  | otherwise = Error "Maximum character limit of 256 exceeded."

isSixDigit :: Int -> Result (Html ()) Int
isSixDigit n
  | n >= 940000 && n <= 999999 = Success n
  | otherwise = Error "The input is incorrect, please review."

checkRequired :: Text -> Result (Html ()) Text
checkRequired txt
  | onull txt = Error $ toHtml ("A value is required" :: Text)
  | otherwise = Success txt

numValidation :: [Ticket [Int]] -> Text -> Result (Html ()) Int
numValidation tickets =
      checkRequired
  >=> readText "Please only enter a number"
  >=> checkDuplicate tickets

checkDuplicate :: [Ticket [Int]] -> Int -> Result (Html ()) Int
checkDuplicate tickets newNumber
  | newNumber `elem` (fmap ticketNumber tickets) = Error "This ticket has already been entered. Please enter a new ticket number."
  | otherwise = Success newNumber
