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

  -- * Core
  email, emails, emailWithTemplate, request,

  -- * Data
  Email (..), TrackLinks (..), Attachment (..),
  EmailWithTemplate (..), defaultEmail, defaultEmailWithTemplate,
  Sent (..),

  -- * Error
  PostmarkError (..), PostmarkErrorType (..),

  -- * Request
  PostmarkRequest (..), PostmarkRequest',

  -- * Response
  PostmarkResponse (..), PostmarkUnexpectedType (..),
  PostmarkResponse', syntaxErr, formatErr,

  -- * Settings
  PostmarkSettings (..), postmarkTestToken,
  postmarkHttpTest, postmarkHttpsTest,
  postmarkHttp, postmarkHttps

) where

import Network.Api.Postmark.Core as X
import Network.Api.Postmark.Data as X hiding (ojson, oljson, omjson, toText)
import Network.Api.Postmark.Error as X
import Network.Api.Postmark.Request as X
import Network.Api.Postmark.Response as X
import Network.Api.Postmark.Settings as X
