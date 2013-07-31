-- |
-- Module:      Network.Api.Postmark
-- Copyright:   (c) 2012 Mark Hibberd
-- License:     BSD3
-- Maintainer:  Mark Hibberd <mark@hibberd.id.au>
-- Portability: portable
--
-- Library for postmarkapp.com HTTP Api.
--
-- To get start see some examples in the 'Network.Api.Postmark.Tutorial' module at <http://hackage.haskell.org/packages/archive/postmark/0.0.2/doc/html/Network-Api-Postmark-Tutorial.html>.
--
-- Source and more information can be found at <https://github.com/apiengine/postmark>.
--
-- To experiment with a live demo try:
-- > $ git clone https://github.com/apiengine/postmark
-- > $ cd postmark
-- > $ cabal install --only-dependencies &&  cabal configure -f demo  && cabal build
-- > $ ./dist/build/postmark-demo/postmark-demo
--
-- Issues can be reported at <https://github.com/apiengine/postmark/issues>.
--
module Network.Api.Postmark (module X) where

import Network.Api.Postmark.Core as X
import Network.Api.Postmark.Data as X hiding (ojson, oljson, omjson, toText)
import Network.Api.Postmark.Error as X
import Network.Api.Postmark.Request as X
import Network.Api.Postmark.Response as X
import Network.Api.Postmark.Settings as X
