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
                                             status401, status200)
import qualified Network.Wai                as Wai

import           Keys                       (googleKey)
import           Network.OAuth.OAuth2

jwtAuth :: IO Wai.Middleware
jwtAuth = do
  manager <- newManager tlsManagerSettings
  return $ \app req respond -> do
    let token = (lookup hAuthorization $ Wai.requestHeaders req) >>= extractBearerToken
    case token of
      Nothing -> app req $ respond401 manager respond req
      Just _ -> app req respond

extractBearerToken :: S.ByteString -> Maybe S.ByteString
extractBearerToken bs =
  if method == "bearer"
     then Just token
     else Nothing
  where (k, v) = S.break isSpace bs
        token = S.dropWhile isSpace v
        method = S.map toLower k

respond401 :: Manager -> (Wai.Response -> IO b) -> Wai.Request -> t -> IO b
respond401 manager sendResponse req _ =
   case Wai.rawPathInfo req of
    "/googleCallback" -> responseJWT
    _                 -> response401
  where response401 = sendResponse $ Wai.responseLBS status401
                      [(hContentType, "text/plain")]
                      $ C.pack $ "Authentication via JWT token is required.\n Please visit " ++ show googleAuthUrl
        responseJWT = do
          let query = Wai.queryString req
          let code = lookup "code" query
          case code of
            Nothing -> sendResponse $ Wai.responseLBS status200 [(hContentType, "text/plain")] "Could not get code"
            Just maybeC -> do
              case maybeC of
                Nothing -> sendResponse $ Wai.responseLBS status200 [(hContentType, "text/plain")] "Could not get code"
                Just c -> do
                  (Right token) <- fetchAccessToken manager googleKey c
                  sendResponse $ Wai.responseLBS
                    status200
                    [(hContentType, "text/plain")]
                    $ C.pack $ "Toll, hier JWT:" ++ show token


googleAuthUrl :: URI
googleAuthUrl = authorizationUrl googleKey `appendQueryParam` googleScopeUserInfo

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.email")]
