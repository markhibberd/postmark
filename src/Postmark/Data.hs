{-# LANGUAGE OverloadedStrings #-}
module Postmark.Data where

import Data.Aeson
import Data.Map
import Data.Text
import Data.Time
{-

Authentication headers
X-Postmark-Server-Token: your-api-key-here

https://api.postmarkapp.com/email

Message format
{
  "From" : "sender@example.com",
  "To" : "receiver@example.com",
  "Cc" : "copied@example.com",
  "Bcc": "blank-copied@example.com",
  "Subject" : "Test",
  "Tag" : "Invitation",
  "HtmlBody" : "<b>Hello</b>",
  "TextBody" : "Hello",
  "ReplyTo" : "reply@example.com",
  "Headers" : [{ "Name" : "CUSTOM-HEADER", "Value" : "value" }]
}

  "Attachments": [
    {
      "Name": "readme.txt",
      "Content": "dGVzdCBjb250ZW50",
      "ContentType": "text/plain"
    },
    {
      "Name": "report.pdf",
      "Content": "dGVzdCBjb250ZW50",
      "ContentType": "application/octet-stream"
    }


{
  "ErrorCode" : 0,
  "Message" : "OK",
  "MessageID" : "b7bc2f4a-e38e-4336-af7d-e6c392c2f817",
  "SubmittedAt" : "2010-11-26T12:01:05.1794748-05:00",
  "To" : "receiver@example.com"
}

https://api.postmarkapp.com/email/batch

You just need to know if your data is valid. You can do that by passing the “POSTMARK_API_TEST” value as your server API token.
-}

type  BatchEmail = [Email]

data Email = Email {
    emailFrom :: [Text]
  , emailTo :: [Text]
  , emailCc :: [Text]
  , emailBcc :: [Text]
  , emailSubject :: Text
  , emailTag :: Text
  , emailHtml :: Maybe Text
  , emailText :: Maybe Text
  , emailReplyTo :: Text
  , emailHeaders :: Map Text Text
  , emailAttachments :: [Attachment]
  }

data Attachment = Attachment {
    attachmentName :: Text
  , attachmentContent :: Text
  , attachmentContentType :: Text
  }

data PostmarkRequest =
    HttpPostmarkRequest Text
  | HttpsPostmarkRequest Text

data PostmarkResponse =
    SuccessPostmarkResponse {
        postmarkMessageId :: Text
      , postmarkSubmittedAt :: UTCTime
      , postmarkTo :: Text
      }
  | UnauthorizedPostmarkResponse
  | UnprocessiblePostmarkResponse PostmarkError
  | ServerErrorPostmarkResponse

data PostmarkError =
    PostmarkBadApiToken
  | PostmarkInvalidEmail
  | PostmarkSenderNotFound
  | PostmarkSenderNotConfirmed
  | PostmarkInvalidJson
  | PostmarkIncompatibleJson
  | PostmarkNotAllowed
  | PostmarkInactive
  | PostmarkBounceNotFound
  | PostmarkBounceQueryException
  | PostmarkJsonRequired
  | PostmarkTooManyMessages
  | PostmarkUnkownError Int
  deriving Eq

instance ToJSON Email where
  toJSON v = object [
      "From" .= emailFrom v
    , "To" .= intercalate "," (emailTo v)
    , "Cc" .= intercalate "," (emailCc v)
    , "Bcc" .= intercalate "," (emailBcc v)
    , "Subject" .= emailSubject v
    , "Tag" .= emailTag v
    , "HtmlBody" .= emailHtml v
    , "TextBody" .= emailText v
    , "ReplyTo" .= emailReplyTo v
    , "Headers" .= emailHeaders v
    , "Attachments" .= emailAttachments v
    ]

instance ToJSON Attachment where
  toJSON v = object [
      "Name" .= attachmentName v
    , "Content" .= attachmentContent v
    , "ContentType" .= attachmentContentType v
    ]

instance Show PostmarkError where
  show PostmarkBadApiToken = "Your request did not submit the correct API token in the X-Postmark-Server-Token header."
  show PostmarkInvalidEmail = "Validation failed for the email request JSON data that you provided."
  show PostmarkSenderNotFound = "You are trying to send email with a From address that does not have a sender signature."
  show PostmarkSenderNotConfirmed = "You are trying to send email with a From address that does not have a corresponding confirmed sender signature."
  show PostmarkInvalidJson = "The JSON input you provided is syntactically incorrect."
  show PostmarkIncompatibleJson = "The JSON input you provided is syntactically correct, but still not the one we expect."
  show PostmarkNotAllowed = "You ran out of credits."
  show PostmarkInactive = "You tried to send to a recipient that has been marked as inactive. Inactive recipients are ones that have generated a hard bounce or a spam complaint."
  show PostmarkBounceNotFound = "You requested a bounce by ID, but we could not find an entry in our database."
  show PostmarkBounceQueryException = "You provided bad arguments as a bounces filter."
  show PostmarkJsonRequired = "Your HTTP request does not have the Accept and Content-Type headers set to application/json."
  show PostmarkTooManyMessages = "Your batched request contains more than 500 messages."
  show (PostmarkUnkownError code) = "An unexpected error code [" ++ show code ++ "] was retured from postmark."

postmarkTestToken :: Text
postmarkTestToken = "POSTMARK_API_TEST"

toPostmarkError :: Int -> PostmarkError
toPostmarkError 0 = PostmarkBadApiToken
toPostmarkError 300 = PostmarkInvalidEmail
toPostmarkError 400 = PostmarkSenderNotFound
toPostmarkError 401 = PostmarkSenderNotConfirmed
toPostmarkError 402 = PostmarkInvalidJson
toPostmarkError 403 = PostmarkIncompatibleJson
toPostmarkError 405 = PostmarkNotAllowed
toPostmarkError 406 = PostmarkInactive
toPostmarkError 407 = PostmarkBounceNotFound
toPostmarkError 408 = PostmarkBounceQueryException
toPostmarkError 409 = PostmarkJsonRequired
toPostmarkError 410 = PostmarkTooManyMessages
toPostmarkError code = PostmarkUnkownError code
