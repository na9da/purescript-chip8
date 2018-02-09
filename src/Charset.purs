module Charset ( loadCharset
               , lookupChar
               , defaultCharset
               ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Data.Array as A
import Data.Foldable (foldl)
import Data.String (toCharArray)
import RefArray (RefArray, splice)
import Types (Addr, Byte, addr, byte, int, plus)

loadCharset
  :: forall e
   . RefArray Byte
  -> Array (Array String)
  -> Eff (ref :: REF | e) Unit
loadCharset mem characters = 
  splice mem (int charsetAddr) (A.length bitmap) bitmap
  where
    bitmap = A.concatMap (\c -> lineToByte <$> c) characters
    lineToByte line = byte $ foldl shift 0 (toCharArray line)
    shift i c = i * 2 + (if c == '*' then 1 else 0)

lookupChar :: Byte -> Addr
lookupChar val = charsetAddr `plus` (5 * int val)

charsetAddr :: Addr
charsetAddr = addr 0x00

defaultCharset :: Array (Array String)
defaultCharset =
  [
        -- 0
        [ "****"
        , "*  *"
        , "*  *"
        , "*  *"
        , "****"
        ]
      -- 1
      , [ "  * "
        , " ** "
        , "  * "
        , "  * "
        , " ***"
        ]
      -- 2
      , [ "****"
        , "   *"
        , "****"
        , "*   "
        , "****"
        ]
      -- 3
      , [ "****"
        , "   *"
        , "****"
        , "   *"
        , "****"
        ]
      -- 4
      , [ "*  *"
        , "*  *"
        , "****"
        , "   *"
        , "   *"
        ]
      -- 5
      , [ "****"
        , "*   "
        , "****"
        , "   *"
        , "****"
        ]
      -- 6
      , [ "****"
        , "*   "
        , "****"
        , "*  *"
        , "****"
        ]
      -- 7
      , [ "****"
        , "   *"
        , "  * "
        , " *  "
        , " *  "
        ]
      -- 8
      , [ "****"
        , "*  *"
        , "****"
        , "*  *"
        , "****"
        ]
      -- 9
      , [ "****"
        , "*  *"
        , "****"
        , "   *"
        , "****"
        ]
      -- a
      , [ "****"
        , "*  *"
        , "****"
        , "*  *"
        , "*  *"
        ]
      -- b
      , [ "*** "
        , "*  *"
        , "*** "
        , "*  *"
        , "*** "
        ]
      -- c
      , [ "****"
        , "*   "
        , "*   "
        , "*   "
        , "****"
        ]
      -- d
      , [ "*** "
        , "*  *"
        , "*  *"
        , "*  *"
        , "*** "
        ]
      -- e
      , [ "****"
        , "*   "
        , "****"
        , "*   "
        , "****"
        ]
      -- f
      , [ "****"
        , "*   "
        , "****"
        , "*   "
        , "*   "
        ]
  ]
