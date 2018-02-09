module Main where

import Prelude

import Chip (Chip, getPtr, getReg, loadRom, newChip, readMem, setReg)
import Chip as C
import Control.Monad.Eff (Eff, untilE)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF)
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Display (Display, clearDisplay, drawSprite, newDisplay)
import Graphics.Canvas (CANVAS, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D)
import IoCommand (IoCommand(..))
import Keys (KEYS, getKey, listenKeys)
import Opcode (Opcode(..))
import Opcode as O
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Base64 (decode64)
import Control.Monad.Eff.Timer (TIMER)
import Timer (startTimer)
import Types (Byte, and, bits, byte, int, reg, strToBytes)
import AnimationFrame (DOM, requestAnimationFrame)

type EmulationEffects e = ( timer :: TIMER
                          , keys :: KEYS
                          , arrayBuffer :: ARRAY_BUFFER
                          , canvas :: CANVAS
                          , random :: RANDOM
                          , ref :: REF
                          , dom :: DOM
                          , console :: CONSOLE | e)

runDisassembly 
    :: forall e
     . Array Byte
    -> Eff (console :: CONSOLE, ref :: REF | e) Unit
runDisassembly rom = do
  chip <- newChip
  loadRom chip rom
  untilE do
    C.fetch chip >>= decode >>=
    case _ of
      Eof -> pure true
      op -> logShow op *> pure false
  where
    decode = pure <<< O.decode 

stepEmulation :: forall e. Chip -> Display -> Int -> Eff (EmulationEffects e) Unit
stepEmulation chip display n = do
   io <- fetch >>= decode >>= debug >>= eval
   case io of
     Next -> next n
     Io ClearDisplay -> do
        clearDisplay display
        next n
     Io (SetRandom r andWith) -> do
        rand <- byte <$> randomInt 0 255
        setReg chip r (rand `and` andWith)
        next n
     Io (DrawSprite r1 r2 height) -> do
        x <- toNumber <$> int <$> getReg chip r1
        y <- toNumber <$> int <$> getReg chip r2
        addr <- getPtr chip
        sprite <- ((<$>) bits) <$> readMem chip addr height
        ovf <- drawSprite display x y sprite
        if ovf
          then setReg chip (reg 0xf) (byte 0x1)
          else setReg chip (reg 0xf) (byte 0x0)
        next n
     Io (GetKey r) -> do
        getKey (\key -> do
                  setReg chip r key
                  next n)
     Io (StartTimer r) -> do
        count <- getReg chip r
        startTimer chip count
        next n
     Io (PlaySound r) -> (getReg chip r >>= logShow) *> next n
     Io op -> unsafeCrashWith ("unhandled Io for opcode " <> show op)
     Exit -> unsafeCrashWith "exited."
   where
     fetch = C.fetch chip
     decode = pure <$> O.decode
     eval = C.eval chip
     next 0 = requestAnimationFrame (stepEmulation chip display 10)
     next n' = stepEmulation chip display (n' - 1)
     debug op = logShow op *> pure op


runEmulation :: forall e. Chip -> Display -> Eff (EmulationEffects e) Unit
runEmulation chip display =
  stepEmulation chip display 10

main :: forall e. Eff (EmulationEffects e) Unit
main = do
  canvas <- (unsafePartial $ fromJust) <$> getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  display <- newDisplay ctx {scaleX: width / 64.0, scaleY: height / 32.0}
  
  let rom = strToBytes ufo
--  runDisassembly rom
      
  chip <- newChip
  loadRom chip rom
  listenKeys chip.keys
  runEmulation chip display
  where
    maze = decode64 "YABhAKIiwgEyAaIe0BRwBDBAEgRgAHEEMSASBBIcgEAgECBAgBA="
    ufo = decode64 "os1pOGoI2aOi0GsAbAPbw6LWZB1lH9RRZwBoDyKiIqxIABIiZB5lHKLT1FNuAGaAbQTtoWb/bQXtoWYAbQbtoWYBNoAi2KLQ28PNAYvU28M/ABKSos3Zo80BPQBt/3n+2aM/ABKMTgASLqLT1FNFABKGdf+EZNRTPwESRm0IjVJNCBKMEpIirHj/Eh4ioncFEpYioncPIqJtA/0YotPUUxKGovj3M2MAIrYA7qL4+DNjMiK2AO5tG/Jl8CnT1XMF8SnT1XMF8inT1QDuAXz+fGDwYEDgoPjUbgFtEP0YAO4="
