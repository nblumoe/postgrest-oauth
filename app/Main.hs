-- |

module Main where

import PostgREST.Config (AppConfig (..),
                         readOptions)
import           PostgREST.DbStructure

import PostgRESTOAuth

import qualified Hasql.Pool                           as P
import           Data.IORef
import           Data.String.Conversions              (cs)

import           Network.Wai.Handler.Warp


main :: IO ()
main = do
  conf <- readOptions
  let port = configPort conf
      pgConfig = cs $ configDatabase conf
      poolConfig = configPool conf
      appSettings = setPort port
        . setServerName  "postgrest"
        $ defaultSettings
  pool <- P.acquire (poolConfig, 10, pgConfig)
  structure <- P.use pool $ getDbStructure (cs $ configSchema conf)
  refDbStructure <- newIORef $ either (error.show) id structure
  putStrLn $ "Listening on port " ++ show port
  runSettings appSettings $ postgrestOAuthApp conf refDbStructure pool
