# purescript-localstorage

Access JS webstorage (local &amp; session) in a typesafe and convenient way.
Just define a key type with some smart constructors and you are done
(GADTs would even make those unnecessary).

The type system then saves you from misspelling keys (no plain
strings) and also makes sure that you can only read the same type as
you wrote before. This is because the keys encode the type of the data
stored with them in the type system.

In addition to type safety, having a key type instead of plain strings
also makes the API easier to use, because with the value type encoded
in the key type, type inference is always possible, even for a plain
`getItem` call. You will never need any type annotations!

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

serverCacheKey :: MyKey ServerCache
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

The generated module documentation is currently less readable than the code, so check it out directly:

https://github.com/eskimor/purescript-localstorage/blob/master/src/Browser/LocalStorage.purs

Have fun!
