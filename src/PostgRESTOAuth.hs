module PostgRESTOAuth
  ( jwtAuth
  ) where

import qualified Data.ByteString       as S
-- needed for JWT handling
-- import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word8            (isSpace, toLower)

import           Network.HTTP.Types    (hAuthorization, hContentType, status401)
import qualified Network.Wai           as Wai

jwtAuth :: Wai.Middleware
jwtAuth app req sendResponse =
  case (lookup hAuthorization $ Wai.requestHeaders req)
    >>= extractBearerToken of
    Nothing -> app req $ respond401 sendResponse
    Just _ -> app req sendResponse

extractBearerToken :: S.ByteString -> Maybe S.ByteString
extractBearerToken bs =
  let (x, y) = S.break isSpace bs
  in if S.map toLower x == "bearer"
     then Just $ S.dropWhile isSpace y
          else Nothing

respond401 :: (Wai.Response -> t1) -> t -> t1
respond401 sendResponse _ = sendResponse $ Wai.responseLBS
  status401
  [(hContentType, "text/plain")]
  "Authentication via JWT token is required."
