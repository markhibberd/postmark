{-# LANGUAGE OverloadedStrings #-}
module Postmark.Demo where

import Postmark

demo :: IO PostmarkResponse
demo = sendEmail $ HttpPostmarkRequest testKey defaultEmail {
    emailFrom = ["demo-from@postmark.hs"]
  , emailTo = ["demo-to@postmark.hs"]
  , emailSubject = "demo, yes it really is a demo"
  , emailTag = Just "demo"
  , emailHtml = Just "Hello world!"
  , emailReplyTo = "demo-reply-to@postmark.hs"
  }

