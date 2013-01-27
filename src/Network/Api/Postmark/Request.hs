{-# LANGUAGE GADTs, GADTSyntax #-}
module Network.Api.Postmark.Request where

import Network.Api.Postmark.Error

import Data.Aeson
import Data.Text

import Network.Api.Support
import Network.HTTP.Types

data PostmarkRequest e a where
  PostmarkRequest :: (FromJSON e, FromJSON a) => StdMethod -> Text -> RequestTransformer IO -> PostmarkRequest e a

type PostmarkRequest' a =
  PostmarkRequest PostmarkError a
