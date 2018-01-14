{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Settings (
  PostmarkSettings (..),
  PostmarkApiToken,
  postmarkTestToken,
  postmarkHttpTest,
  postmarkHttpsTest,
  postmarkHttp,
  postmarkHttps
) where

import Data.Text

type PostmarkApiToken = Text

data PostmarkSettings =
  PostmarkSettings {
      apiUrl :: Text
    , apiToken :: PostmarkApiToken
    } deriving (Eq, Show)

-- | An API token that you can use when you want to send test emails that
-- don't actually get delivered to the recipient.
--
-- https://postmarkapp.com/developer/api/overview#authentication
postmarkTestToken :: PostmarkApiToken
postmarkTestToken = "POSTMARK_API_TEST"

postmarkHttpTest :: PostmarkSettings
postmarkHttpTest = postmarkHttp postmarkTestToken

postmarkHttpsTest :: PostmarkSettings
postmarkHttpsTest = postmarkHttps postmarkTestToken

postmarkHttp :: PostmarkApiToken -> PostmarkSettings
postmarkHttp = PostmarkSettings "http://api.postmarkapp.com"

postmarkHttps :: PostmarkApiToken -> PostmarkSettings
postmarkHttps = PostmarkSettings "https://api.postmarkapp.com"
