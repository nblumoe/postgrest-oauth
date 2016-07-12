-- |

module Main where

import           PostgREST.App
import           PostgREST.Config         (AppConfig (..), readOptions)
import           PostgREST.DbStructure

import           PostgRESTOAuth

import           Data.IORef
import           Data.String.Conversions  (cs)
import qualified Hasql.Pool               as P
import           Network.Wai.Handler.Warp


main :: IO ()
main = do
  conf <- readOptions

  let port = configPort conf
      pgConfig = cs $ configDatabase conf
      poolConfig = configPool conf
      appSettings = setPort port . setServerName  "postgrest" $ defaultSettings

  pool <- P.acquire (poolConfig, 10, pgConfig)
  dbStructure <- P.use pool $ getDbStructure (cs $ configSchema conf)
  refDbStructure <- newIORef $ either (error.show) id dbStructure

  putStrLn $ "Listening on port " ++ show port
  app <- jwtAuth
  runSettings appSettings $ app $ postgrest conf refDbStructure pool
