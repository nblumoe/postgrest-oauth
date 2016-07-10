module PostgRESTOAuth
  ( jwtAuth
  ) where

import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy.Char8 as C
-- needed for JWT handling
-- import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word8                 (isSpace, toLower)

import           Network.HTTP.Conduit
import           Network.HTTP.Types         (hAuthorization, hContentType,
                                             status401)
import qualified Network.Wai                as Wai

import           Keys                       (googleKey)
import           Network.OAuth.OAuth2

jwtAuth :: Wai.Middleware
jwtAuth app req sendResponse =
  case lookup hAuthorization headers >>= extractBearerToken of
    Nothing -> app req $ respond401 sendResponse
    Just _ -> app req sendResponse
  where headers = Wai.requestHeaders req
        manager = newManager tlsManagerSettings

extractBearerToken :: S.ByteString -> Maybe S.ByteString
extractBearerToken bs =
  if method == "bearer"
     then Just token
     else Nothing
  where (k, v) = S.break isSpace bs
        token = S.dropWhile isSpace v
        method = S.map toLower k

respond401 :: (Wai.Response -> t1) -> t -> t1
respond401 sendResponse _ = sendResponse $ Wai.responseLBS
  status401
  [(hContentType, "text/plain")]
  $ C.pack $ "Authentication via JWT token is required.\n Please visit " ++ show googleAuthUrl

googleAuthUrl = authorizationUrl googleKey `appendQueryParam` googleScopeUserInfo

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.email")]
