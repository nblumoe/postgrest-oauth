module PostgRESTOAuth
  ( postgrestOAuthApp
  ) where

import qualified Hasql.Pool as H

import GHC.IORef
import PostgREST.App as PGR
import PostgREST.Config as PGR
import PostgREST.Types as PGR
import qualified Network.Wai                    as Wai

postgrestOAuthApp :: PGR.AppConfig
  -> IORef PGR.DbStructure
  -> H.Pool
  -> Wai.Application
postgrestOAuthApp conf refDbStructure pool =
  postgrest conf refDbStructure pool
