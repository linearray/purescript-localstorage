# purescript-localstorage
Access JS webstorage (local &amp; session) in a typesafe and convenient way:

Find a basic usage example in test/Main.purs, repeated here for convenience:

```purescript
module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic

import Browser.LocalStorage


-- Define a key type:

data MyKey a = UserConfigKey
            | ServerCacheKey

derive instance genericMyKey :: Generic (MyKey a)

-- "Smart" constructors as replacement for GADTs:
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

-- Data to store:
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


-- Actually use it (requires a browser):
main :: forall e. Eff (console :: CONSOLE, storage :: STORAGE | e) Unit
main = do
  localStorage.setItem userConfigKey (UserConfig {userName : "Bob", email : "bob@bob.com"})
  user <- localStorage.getItem userConfigKey
  log $ gShow user

```

Doc generation currently fails for me with an internal error, so just checkout:

https://github.com/eskimor/purescript-localstorage/blob/master/src/Browser/LocalStorage.purs

Have fun!
