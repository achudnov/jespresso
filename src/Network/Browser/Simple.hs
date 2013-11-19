-- | A wrapper around Network.HTTP to present a simpler interface for
-- requesting pages via HTTP
module Network.Browser.Simple (download) where

import Network.URI
import Network.HTTP
import Network.Browser
import Network.HTTP.Encoding
import Data.ByteString.Lazy

-- | Requests a web page specified by the URI, including optional
-- cookies. Returns the contents of the page as a @String@ and new
-- cookies. Follows redirects, decodes the response body, if
-- possible. Can fail if decoding is impossible.
download :: URI -> [Cookie] -> IO (String, [Cookie])
download uri cookies = 
  browse $ do setAllowRedirects True
              setUserAgent "Mozilla/5.0 (compatible; jespresso/1.0)"
              setCookies cookies
              (_, rsp) <- request (defaultGETRequest_ uri :: Request ByteString)
              cks <- getCookies
              return (rsp, cks)
  >>= \(rsp, cks) -> case decodeBody rsp of
               Left msg  -> fail $ show msg
               Right res -> return (decodedBody res, cks)
