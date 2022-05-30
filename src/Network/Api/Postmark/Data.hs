{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings, GADTSyntax #-}
module Network.Api.Postmark.Data (

  -- * Request types

  -- ** Email
  Email (..),
  defaultEmail,

  -- ** Email with template
  EmailWithTemplate (..),
  defaultEmailWithTemplate,

  -- ** Track links
  TrackLinks (..),

  -- ** Attachment
  Attachment (..),

  -- ** Response type
  Sent (..),

  -- * Internal Json tools
  ojson,
  oljson,
  omjson,
  toText
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import Data.Aeson
import Data.Map as M
import Data.Maybe
import Data.Text as T hiding (null)
import Data.List as L

-- * Request types

-- | Email data type. It is recommended that you use the defaultEmail
--   function and selector syntax to build an email, e.g.:
--
-- > defaultEmail {
-- >     emailFrom = "you@yourdomain.com"
-- >   , emailTo = "person@example.com"
-- >   , emailSubject = "This is an example email!"
-- >   }
data Email = Email {
    emailFrom :: Text
  , emailTo :: [Text]
  , emailCc :: [Text]
  , emailBcc :: [Text]
  , emailSubject :: Text
  , emailTag :: Maybe Text
  , emailHtml :: Maybe Text
  , emailText :: Maybe Text
  , emailReplyTo :: Text
  , emailHeaders :: Map Text Text
  , emailTrackOpens :: Maybe Bool
  , emailTrackLinks :: Maybe TrackLinks
  , emailAttachments :: [Attachment]
  }

-- | When “link tracking” is enabled, Postmark will record statistics when a
-- user clicks on a link in an email. You can use this feature to determine
-- if a particular recipient has clicked a link that was emailed to them.
--
-- https://postmarkapp.com/developer/user-guide/tracking-links#enabling-link-tracking
data TrackLinks
  = None        -- ^ No links will be replaced or tracked.
  | HtmlAndText -- ^ Links will be replaced in both HTML and text bodies.
  | HtmlOnly    -- ^ Links will be replaced in only the HTML body. You may
                --   want this option if you do not want encoded tracking
                --   links to appear in the plain text of an email.
  | TextOnly    -- ^ Links will be replaced in only the text body.
  deriving (Show)

data Attachment = Attachment {
    attachmentName :: Text
  , attachmentContent :: Text
  , attachmentContentType :: Text
  }

data EmailWithTemplate = EmailWithTemplate {
    templateId :: Int
  , templateModel :: Map Text Text
  , inlineCss :: Bool
  , emailFrom' :: Text
  , emailTo' :: [Text]
  , emailCc' :: [Text]
  , emailBcc' :: [Text]
  , emailTag' :: Maybe Text
  , emailReplyTo' :: Text
  , emailHeaders' :: Map Text Text
  , emailTrackOpens' :: Maybe Bool
  , emailTrackLinks' :: Maybe TrackLinks
  , emailAttachments' :: [Attachment]
  }

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
  , emailTrackOpens = Nothing
  , emailTrackLinks = Nothing
  , emailAttachments = []
  }

defaultEmailWithTemplate :: EmailWithTemplate
defaultEmailWithTemplate = EmailWithTemplate {
    templateId = 0
  , templateModel = M.empty
  , inlineCss = False
  , emailFrom' = ""
  , emailTo' = []
  , emailCc' = []
  , emailBcc' = []
  , emailTag' = Nothing
  , emailReplyTo' = ""
  , emailHeaders' = M.empty
  , emailTrackOpens' = Nothing
  , emailTrackLinks' = Nothing
  , emailAttachments' = []
  }

instance ToJSON Email where
  toJSON v = object ([
      "From" .= (emailFrom v)
    , "To" .= T.intercalate "," (emailTo v)
    , "Subject" .= emailSubject v
    , "ReplyTo" .= emailReplyTo v
    ] ++ catMaybes [
      ojson "HtmlBody" (emailHtml v)
    , ojson "TextBody" (emailText v)
    , ojson "Tag" (emailTag v)
    , oljson "Cc" (emailCc v) (T.intercalate ",")
    , oljson "Bcc" (emailBcc v) (T.intercalate ",")
    , omjson "Headers" (emailHeaders v)
    , ojson "TrackOpens" (emailTrackOpens v)
    , ojson "TrackLinks" (emailTrackLinks v)
    , oljson "Attachments" (emailAttachments v) id
    ])

{- The reason we are being explicit here is because the serialized constructors
   for TrackLinks match the possible values in the Postmark API.
   We don't want to send values wholesale if new constructors come along.
-}
instance ToJSON TrackLinks where
  toJSON v =
    toJSON $ case v of
      None ->
        "None" :: Text
      HtmlAndText ->
        "HtmlAndText"
      HtmlOnly ->
        "HtmlOnly"
      TextOnly ->
        "TextOnly"

instance ToJSON Attachment where
  toJSON v = object [
      "Name" .= attachmentName v
    , "Content" .= attachmentContent v
    , "ContentType" .= attachmentContentType v
    ]

instance ToJSON EmailWithTemplate where
  toJSON v = object ([
      "TemplateId" .= templateId v
    , "TemplateModel" .= templateModel v
    , "From" .= (emailFrom' v)
    , "To" .= T.intercalate "," (emailTo' v)
    , "ReplyTo" .= emailReplyTo' v
    ] ++ catMaybes [
      ojson "Tag" (emailTag' v)
    , oljson "Cc" (emailCc' v) (T.intercalate ",")
    , oljson "Bcc" (emailBcc' v) (T.intercalate ",")
    , omjson "Headers" (emailHeaders' v)
    , ojson "TrackOpens" (emailTrackOpens' v)
    , ojson "TrackLinks" (emailTrackLinks' v)
    , oljson "Attachments" (emailAttachments' v) id
    ])

-- * Response types

data Sent =
  Sent {
      postmarkMessageId :: Text
    , postmarkSubmittedAt :: Text
    , postmarkTo :: Text
    } deriving (Eq, Show)

instance FromJSON Sent where
  parseJSON (Object o) = Sent
    <$> o .: "MessageID"
    <*> o .: "SubmittedAt"
    <*> o .: "To"
  parseJSON _ = fail "Invalid `Sent` Json"


-- * Internal Json tools

#if ! MIN_VERSION_aeson(2,0,0)
type Key = Text
#endif

ojson :: ToJSON a => Key -> Maybe a -> Maybe (Key, Value)
ojson k = fmap (k .=)

oljson :: ToJSON b => Key -> [a] -> ([a] -> b) -> Maybe (Key, Value)
oljson k vs f = if L.null vs then Nothing else Just (k .= f vs)

omjson :: (ToJSON a) => Key -> Map Text a -> Maybe (Key, Value)
omjson k vs = if M.null vs then Nothing else Just (k .= vs)

toText :: BL.ByteString -> Text
toText = LT.toStrict . LE.decodeUtf8
