{-# LANGUAGE OverloadedStrings #-}
module Network.Api.Postmark.Demo where

import Network.Api.Postmark

demo :: IO (PostmarkResponse' Sent)
demo = request postmarkHttpsTest $ email defaultEmail {
    emailFrom = "demo-from@postmark.hs"
  , emailTo = ["demo-to@postmark.hs"]
  , emailSubject = "demo, yes it really is a demo"
  , emailTag = Just "demo"
  , emailHtml = Just "Hello world!"
  , emailReplyTo = "demo-reply-to@postmark.hs"
  }
