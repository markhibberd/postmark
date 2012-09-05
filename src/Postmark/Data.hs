module Postmark.Data where

import Data.Text
import Data.Aeson
import Data.Map

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

data PostmarkRequest =
    HttpPostmarkRequest Text
  | HttpsPostmarkRequest Text

data PostmarkResponse =
    SuccessPostmarkResponse
  | UnauthorizedPostmarkResponse
  | UnprocessiblePostmarkResponse
  | ServerErrorPostmarkResponse

{-
0 – Bad or missing API token
    Your request did not submit the correct API token in the X-Postmark-Server-Token header.
300 – Invalid email request
    Validation failed for the email request JSON data that you provided.
400 – Sender signature not found
    You are trying to send email with a From address that does not have a sender signature.
401 – Sender signature not confirmed
    You are trying to send email with a From address that does not have a corresponding confirmed sender signature.
402 – Invalid JSON
    The JSON input you provided is syntactically incorrect.
403 – Incompatible JSON
    The JSON input you provided is syntactically correct, but still not the one we expect.
405 – Not allowed to send
    You ran out of credits.
406 – Inactive recipient
    You tried to send to a recipient that has been marked as inactive. Inactive recipients are ones that have generated a hard bounce or a spam complaint.
407 – Bounce not found
    You requested a bounce by ID, but we could not find an entry in our database.
408 – Bounce query exception
    You provided bad arguments as a bounces filter.
409 – JSON required
    Your HTTP request does not have the Accept and Content-Type headers set to application/json.
410 – Too many batch messages
    Your batched request contains more than 500 messages.
-}

type BatchEmail = [Email]

data Email = Email {
    emailFrom :: [Text]
  , emailTo :: [Text]
  , emailCc :: [Text]
  , emailBcc :: [Text]
  , emailSubject :: Text
  , emailTag :: Text
  , emailHtml :: Text
  , emailText :: Text
  , emailReplyTo :: Text
  , emailHeaders :: Map Text Text
  , emailAttachments :: [Attachment]
  }

data Attachment = Attachment {
    attachmentName :: Text
  , attachmentContent :: Text
  , attachmentContentType :: Text
  }


postmarkTestToken :: Text
postmarkTestToken = POSTMARK_API_TEST
