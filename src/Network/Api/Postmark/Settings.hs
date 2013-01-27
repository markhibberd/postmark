{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Settings where

import Data.Text

data PostmarkSettings =
  PostmarkSettings {
      apiUrl :: Text
    , apiToken :: Text
    }

postmarkTestToken :: Text
postmarkTestToken = "POSTMARK_API_TEST"

postmarkHttpTest :: PostmarkSettings
postmarkHttpTest = postmarkHttp postmarkTestToken

postmarkHttpsTest :: PostmarkSettings
postmarkHttpsTest = postmarkHttps postmarkTestToken

postmarkHttp :: Text -> PostmarkSettings
postmarkHttp = PostmarkSettings "http://api.postmarkapp.com"

postmarkHttps :: Text -> PostmarkSettings
postmarkHttps = PostmarkSettings "https://api.postmarkapp.com"
