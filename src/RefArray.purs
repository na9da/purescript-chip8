module RefArray ( RefArray
                , newRefArray
                , getRefArray
                , index
                , updateAt
                , splice
                , slice
                , push
                , pop
                ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.ST (pureST)
import Data.Array as A
import Data.Array.ST (freeze, pushSTArray, spliceSTArray, thaw, withArray)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


newtype RefArray a = RefArray (Ref (Array a))

newRefArray :: forall a e. Array a -> Eff (ref :: REF | e) (RefArray a)
newRefArray array = (pure <<< RefArray) =<< newRef array

getRefArray :: forall a e. RefArray a -> Eff (ref :: REF | e) (Array a)
getRefArray (RefArray ref) = readRef ref

splice
  :: forall a e
   . RefArray a
  -> Int
  -> Int
  -> Array a
  -> Eff (ref :: REF | e) Unit
splice (RefArray ref) offset del src = do
  array <- readRef ref
  writeRef ref
    (pureST (withArray (\a -> spliceSTArray a offset del src) array))

slice
  :: forall a eff
   . RefArray a
  -> Int
  -> Int
  -> Eff (ref :: REF | eff) (Array a)
slice (RefArray ref) offset length = do
  array <- readRef ref
  pure (A.slice offset (offset + length) array)
    
index :: forall a e. RefArray a -> Int -> Eff (ref :: REF | e) a
index (RefArray ref) pos = do
  array <- readRef ref
  pure (unsafePartial (A.unsafeIndex array pos))

updateAt
    :: forall a e
     . RefArray a
    -> Int
    -> a
    -> Eff (ref :: REF | e) Unit
updateAt refArray pos val = splice refArray pos 1 [val]


push :: forall a e. RefArray a -> a -> Eff (ref :: REF | e) Unit
push (RefArray ref) item = do
  array <- readRef ref
  let array' = pureST (withArray (\a -> pushSTArray a item) array)
  writeRef ref array'
  
pop :: forall a e. RefArray a -> Eff (ref :: REF | e) a
pop (RefArray ref) = do
  array <- readRef ref
  let (Tuple newArray popped) = popST array
  writeRef ref newArray
  pure popped
  where
    popST array = unsafePartial $
      pureST do 
        st <- thaw array
        [popped] <- spliceSTArray st (A.length array - 1) 1 []
        array' <- freeze st
        pure (Tuple array' popped)

