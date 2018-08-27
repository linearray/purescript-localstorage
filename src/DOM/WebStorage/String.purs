module DOM.WebStorage.String
( Updated
, length
, key
, getItem
, setItem
, removeItem
, clear
--, getItemVar
--, getItemVar'
) where

import Effect.Uncurried
import Effect
import Data.Maybe (Maybe(..), maybe)
import Prelude    (Unit, identity, (<$>), (<<<))

import DOM.WebStorage.ForeignStorage (ForeignStorage)

type Updated s b = { newValue :: s, returnValue :: b }

length :: ForeignStorage -> Effect Int
length = runEffectFn1 lengthImpl

key :: ForeignStorage -> Int -> Effect (Maybe String)
key = runEffectFn4 keyImpl Nothing Just

getItem :: ForeignStorage -> String -> Effect (Maybe String)
getItem = runEffectFn4 getItemImpl Nothing Just

setItem :: ForeignStorage -> String -> String -> Effect Unit
setItem = runEffectFn3 setItemImpl

removeItem :: ForeignStorage -> String -> Effect Unit
removeItem = runEffectFn2 removeItemImpl
 
clear :: ForeignStorage -> Effect Unit
clear = runEffectFn1 clearImpl

-- getItemVar :: ForeignStorage -> String -> String -> Var String
-- getItemVar storage key' = getItemVar' storage key' id id

-- getItemVar' :: ForeignStorage -> String
--   -> (a -> String) -> (String -> a) -> a -> Var a
-- getItemVar' storage key' encode decode defaultItem = makeVar getItem' setItem'
--   where
--     getItem' = maybe defaultItem decode <$> getItem storage key'
--     setItem' = setItem storage key' <<< encode

foreign import lengthImpl       :: EffectFn1 ForeignStorage Int
foreign import keyImpl          :: EffectFn4 (Maybe String) (String -> Maybe String) ForeignStorage Int (Maybe String)
foreign import getItemImpl      :: EffectFn4 (Maybe String) (String -> Maybe String) ForeignStorage String (Maybe String)
foreign import setItemImpl      :: EffectFn3 ForeignStorage String String Unit
foreign import removeItemImpl   :: EffectFn2 ForeignStorage String Unit
foreign import clearImpl        :: EffectFn1 ForeignStorage Unit
