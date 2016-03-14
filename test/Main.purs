module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic

import Browser.LocalStorage



data MyKey a = UserConfigKey
            | ServerCacheKey

derive instance genericMyKey :: Generic (MyKey a)

userConfigKey :: MyKey UserConfig
userConfigKey = UserConfigKey


serverCacheKey :: MyKey UserConfig
serverCacheKey = ServerCacheKey

-- With GADTs this would be even nicer:
{--
data MyKey a where
  UserConfig :: MyKey UserConfig
  ServerCache :: MyKey ServerCache
--}


newtype UserConfig = UserConfig {
  userName :: String
  , email :: String
}
derive instance genericUserConfig :: Generic UserConfig

newtype ServerCache = ServerCache {
  serverData :: String
, otherData :: Int
}

derive instance genericServerCache :: Generic ServerCache



main :: forall e. Eff (console :: CONSOLE, storage :: STORAGE | e) Unit
main = do
  localStorage.setItem userConfigKey (UserConfig {userName : "Bob", email : "bob@bob.com"})
  user <- localStorage.getItem userConfigKey
  log $ gShow user
