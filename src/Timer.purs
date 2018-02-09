module Timer ( startTimer
             ) where

import Prelude

import Chip (Chip)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Types (Byte, byte, minus)

startTimer :: forall e. Chip -> Byte -> Eff (ref :: REF, timer :: TIMER | e) Unit
startTimer chip count' = do
  setTimer chip count'
  void $ setTimeout ms countDown
  where
    countDown = do
      count <- getTimer chip
      if (count == byte 0)
        then pure unit
        else do
          setTimer chip (count `minus` 1)
          void $ setTimeout ms countDown

    ms = 1000 `div` 1000
    
    setTimer {timer} = writeRef timer
    getTimer {timer} = readRef timer
