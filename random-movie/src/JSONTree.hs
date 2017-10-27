{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module JSONTree where

import ClassyPrelude
import Data.Aeson
import Control.Arrow.ArrowList
import Control.Arrow
import Control.Arrow.ArrowIf

hasKey :: ArrowList a => Text -> a Value Value
hasKey k = arrL (hasKey_ k)

hasKey_ :: Text -> Value -> [Value]
hasKey_ k obj@(Object o) = case lookup k o of
  Nothing -> []
  Just _ -> [obj]
hasKey_ _ _ = []

hasKeyValue :: ArrowList a => Text -> (Text -> Bool) -> a Value Value
hasKeyValue k cmp = arrL (hasKeyValue_ k cmp)

hasKeyValue_ :: Text -> (Text -> Bool) -> Value -> [Value]
hasKeyValue_ k cmp val@(Object o) = case lookItUp of
  Nothing -> []
  Just v -> [v]
  where
    lookItUp = do
      v <- lookup k o
      case v of
        String txt ->
          case cmp txt of
            True -> Just val
            False -> Nothing
        _ -> Nothing
hasKeyValue_ _ _ _ = []

getChildren_ :: Value -> [Value]
getChildren_ (Object o) = fmap snd $ mapToList o
getChildren_ (Array arr) = toList arr
getChildren_ _ = []

getKeyValue :: ArrowList a => Text -> a Value Value
getKeyValue k = arrL (getKeyValue_ k)

getKeyValue_ :: Text -> Value -> [Value]
getKeyValue_ k (Object o) = case lookup k o of
  Nothing -> []
  Just v -> [v]
getKeyValue_ _ _ = []

(//>) :: ArrowIf cat => cat a Value -> cat Value Value -> cat a Value
(//>) a b = a >>> deep_ b

infixl 5 //>

deep_ :: ArrowIf a => a Value Value -> a Value Value
deep_ f = f
             `orElse`
             (arrL getChildren_ >>> deep_ f)
