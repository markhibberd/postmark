-- |
-- Module:      Network.Api.Postmark
-- Copyright:   (c) 2012 Mark Hibberd
-- License:     BSD3
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>
-- Portability: portable
--
-- Library for postmarkapp.com HTTP Api.
--
-- To get start see some examples in the "Network.Api.Postmark.Tutorial" module.
--
-- Source and more information can be found at <https://github.com/apiengine/postmark>.
--
-- To experiment with a live demo try:
--
-- > $ git clone https://github.com/apiengine/postmark
-- > $ cd postmark
-- > $ cabal install --only-dependencies &&  cabal configure -f demo  && cabal build
-- > $ ./dist/build/postmark-demo/postmark-demo
--
-- Issues can be reported at <https://github.com/apiengine/postmark/issues>.
--
module Network.Api.Postmark (

  -- * Settings
  PostmarkSettings (..), PostmarkApiToken, postmarkHttp, postmarkHttps,
  -- ** Using the test token
  postmarkTestToken, postmarkHttpTest, postmarkHttpsTest,

  -- * Sending email
  email, emails, Email (..), defaultEmail,

  -- ** Using a template
  emailWithTemplate, EmailWithTemplate (..), defaultEmailWithTemplate,

  -- ** Ancillary types
  TrackLinks (..), Attachment (..), Sent (..),

  -- * Error types
  PostmarkError (..), PostmarkErrorType (..),

  -- * Lower-level API

  -- ** Request
  request, PostmarkRequest (..), PostmarkRequest',

  -- ** Response
  PostmarkResponse (..), PostmarkUnexpectedType (..),
  PostmarkResponse', syntaxErr, formatErr,

) where

import Network.Api.Postmark.Core as X
import Network.Api.Postmark.Data as X hiding (ojson, oljson, omjson, toText)
import Network.Api.Postmark.Error as X
import Network.Api.Postmark.Request as X
import Network.Api.Postmark.Response as X
import Network.Api.Postmark.Settings as X
