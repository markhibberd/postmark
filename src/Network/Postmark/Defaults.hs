{-# LANGUAGE OverloadedStrings #-}
module Network.Postmark.Defaults where

import qualified Data.Map as M
import Data.Text

import Network.Postmark.Data

testKey :: Text
testKey = "POSTMARK_API_TEST"

defaultEmail :: Email
defaultEmail = Email {
    emailFrom = ""
  , emailTo = []
  , emailCc = []
  , emailBcc = []
  , emailSubject = ""
  , emailTag = Nothing
  , emailHtml = Nothing
  , emailText = Nothing
  , emailReplyTo = ""
  , emailHeaders = M.empty
  , emailAttachments = []
  }
