module Example.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic (class Generic, gShow)
import Data.Maybe (maybe)
import DOM (DOM)

import Browser.LocalStorage (GTranscode(..), STORAGE, getLocalStorage)

-- keys type
data MyKey a = UserConfigKey | ServerCacheKey
derive instance genericMyKey :: Generic (MyKey a)

-- if we had GADTs
{--
data MyKey a where
  UserConfig :: MyKey UserConfig
  ServerCache :: MyKey ServerCache
--}

-- "smart constructors" instead
userConfigKey :: MyKey (GTranscode UserConfig)
userConfigKey = UserConfigKey

serverCacheKey :: MyKey (GTranscode ServerCache)
serverCacheKey = ServerCacheKey

-- item types
newtype UserConfig = UserConfig { userName :: String , email :: String }
derive instance genericUserConfig :: Generic UserConfig

newtype ServerCache = ServerCache { serverData :: String , otherData :: Int }
derive instance genericServerCache :: Generic ServerCache

-- requires browser
main :: forall e. Eff (storage :: STORAGE, dom :: DOM, console :: CONSOLE | e) Unit
main = do
  let user = UserConfig { userName : "Bob", email : "bob@bob.com" }
  localStorage <- getLocalStorage
  localStorage.setItem userConfigKey (GTranscode user)
  result <- localStorage.getItem userConfigKey
  log $ maybe "Key not found" gShow result
