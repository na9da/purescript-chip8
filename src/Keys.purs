module Keys ( KEYS
            , listenKeys
            , getKey
            ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF)
import Data.Maybe (Maybe(..), maybe)
import RefArray (RefArray, updateAt)
import Types (Byte, byte)

foreign import data KEYS :: Effect

foreign import onKeyDown
  :: forall e
   . (Int -> Eff (keys :: KEYS | e) Unit)
  -> Eff (keys :: KEYS | e) Unit

foreign import onKeyUp
  :: forall e
   . (Int -> Eff (keys :: KEYS | e) Unit)
  -> Eff (keys :: KEYS | e) Unit

foreign import getKeyImpl
  :: forall e
   . (Int -> Eff (keys :: KEYS | e) Unit)
  -> Eff (keys :: KEYS | e) Unit

mapKey :: Int -> Maybe Int
mapKey c | c >= 48 && c <= 57 = Just (c - 48)
mapKey c | c >= 65 && c <= 70 = Just (10 + (c - 65))
mapKey c | c >= 97 && c <= 102 = Just (10 + (c - 97))
mapKey _ = Nothing


listenKeys 
    :: forall e
     . RefArray Boolean
    -> Eff (ref :: REF, keys :: KEYS | e) Unit
listenKeys keys = do
    onKeyDown (mapKey >>> pressKey)
    onKeyUp (mapKey >>> releaseKey)
    where
      pressKey Nothing = pure unit
      pressKey (Just k) = updateAt keys k true

      releaseKey Nothing = pure unit
      releaseKey (Just k) = updateAt keys k false

getKey
  :: forall e
   . (Byte -> Eff (keys :: KEYS | e) Unit) 
  -> Eff (keys :: KEYS | e) Unit
getKey f = 
    getKeyImpl (mapKey >>> maybe (pure unit) (byte >>> f))
