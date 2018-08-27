# purescript-localstorage

[![Build Status](https://travis-ci.org/eskimor/purescript-localstorage.svg?branch=master)](https://travis-ci.org/eskimor/purescript-localstorage)

Access JS webstorage (local &amp; session) in a type-safe and convenient way.
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

The values you want to store should have a Generic instance, then
everything just works. Alternatively, if your type is not `Generic`, you can provide
`EncodeJson` and `DecodeJson` instances and use `getItem`, `setItem` and friends
from `DOM.WebStorage.JSON`.

Find a basic usage example in test/Example.purs, repeated here for convenience:

```purescript
module Example.Main where

import Prelude

import Control.Monad.Eff            (Eff)
import Control.Monad.Eff.Console    (log)
import Data.Generic.Rep             (class Generic)
import Data.Generic.Rep.Show        (genericShow)
import Data.Maybe                   (maybe)

import DOM.WebStorage               (getItem, setItem, getLocalStorage)

-- 1. Define item types to store.
newtype UserConfig = UserConfig { name :: String, email :: String }

-- TODO: manually implement codecs formatted as "name <email>".
derive instance genericUserConfig :: Generic UserConfig _

newtype ServerCache = ServerCache { lastModified :: Number, recentContacts :: Array UserConfig }

-- 2. Either implement EncodeJson and DecodeJson or derive Generic for (de)serialization.
derive instance genericServerCache :: Generic ServerCache _


-- 3. Define keys as a "phantom type".
data ExampleKey a = UserConfigKey 
                  | ServerCacheKey

derive instance genericExampleKey :: Generic (ExampleKey a) _


-- 4. Make "smart constructor" for each key.
userConfigKey :: ExampleKey UserConfig
userConfigKey = UserConfigKey

serverCacheKey :: ExampleKey ServerCache
serverCacheKey = ServerCacheKey


{- 5. Optionally ask the tooth fairy for a GADT under your pillow.
data ExampleKey a where
  UserConfig  :: ExampleKey UserConfig
  ServerCache :: ExampleKey ServerCache
-}

-- 6. Run in browser.
main :: Effect Unit
main = do
  let userConfig = UserConfig { name : "PureScript", email : "purescript@example.com" }
  localStorage <- getLocalStorage
  setItem localStorage userConfigKey userConfig
  result <- getItem localStorage userConfigKey
  log $ maybe "Are you Private Browsing in Safari?" genericShow result
```

Have fun!
