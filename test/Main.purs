module Test.Main where

import Prelude

import Chip (Chip, getPc, getReg, newChip, popStack, pushStack, readMem, setPtr, setReg, writeMem)
import Chip as C
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, readRef)
import Data.Array ((..))
import Data.Functor (mapFlipped)
import Opcode (Opcode(..))
import Partial.Unsafe (unsafeCrashWith)
import RefArray (slice)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Types (Addr, addr, byte, plus, reg)

type TestEffects e = ( ref :: REF
                     , avar :: AVAR
                     , testOutput :: TESTOUTPUT
                     , console :: CONSOLE | e)

getPtr :: forall e. Chip -> Eff (ref :: REF | e) Addr
getPtr {ptr} = readRef ptr

eval :: forall e. Chip -> Opcode -> Eff (ref :: REF | e) Unit
eval chip opcode = void $ C.eval chip opcode

should :: forall f a b. Functor f => f a -> (a -> b) -> f b
should = mapFlipped

shouldEql :: forall f a b. Functor f => Eq a => f a -> a -> f Boolean
shouldEql fa a = fa `should` ((==) a)

and :: forall f. Applicative f => f Boolean -> f Boolean -> f Boolean
and = lift2 ((&&))

test_Return :: forall e. Eff (ref :: REF | e) Boolean
test_Return = do
  chip <- newChip
  pushStack chip (addr 0x123)
  eval chip (Return)
  getPc chip `shouldEql` addr 0x123

test_Jump :: forall e. Eff (ref :: REF | e) Boolean
test_Jump = do
  chip <- newChip
  eval chip (Jump (addr 0x234))
  getPc chip `shouldEql` addr 0x234

test_Call :: forall e. Eff (ref :: REF | e) Boolean
test_Call = do
  chip <- newChip
  pc <- getPc chip
  eval chip (Call (addr 0x123))
  (getPc chip `shouldEql` addr 0x123) `and` (popStack chip `shouldEql` pc)

test_SkipIfEqImm :: forall e. Eff (ref :: REF | e) Boolean
test_SkipIfEqImm = do
  chip <- newChip
  pc <- getPc chip
  setReg chip (reg 1) (byte 0x42)
  eval chip (SkipIfEqImm (reg 1) (byte 0x42))
  getPc chip `shouldEql` (pc `plus` 2)

test_SkipIfNotEqImm :: forall e. Eff (ref :: REF | e) Boolean
test_SkipIfNotEqImm = do
  chip <- newChip
  pc <- getPc chip
  setReg chip (reg 1) (byte 0x42)
  eval chip (SkipIfNotEqImm (reg 1) (byte 0x12))
  getPc chip `shouldEql` (pc `plus` 2)

test_MoveImm :: forall e. Eff (ref :: REF | e) Boolean
test_MoveImm = do
  chip <- newChip
  eval chip (MoveImm (reg 5) (byte 0x25))
  getReg chip (reg 5) `shouldEql` byte 0x25

test_AddImm :: forall e. Eff (ref :: REF | e) Boolean
test_AddImm = do
  chip <- newChip
  setReg chip (reg 1) (byte 0x42)
  eval chip (AddImm (reg 1) (byte 0x10))
  getReg chip (reg 1) `shouldEql` byte 0x52

test_AddImm_withOverflow :: forall e. Eff (ref :: REF | e) Boolean
test_AddImm_withOverflow = do
  chip <- newChip
  setReg chip (reg 1) (byte 0xff)
  eval chip (AddImm (reg 1) (byte 0x3))
  getReg chip (reg 1) `shouldEql` byte 0x2

test_Move :: forall e. Eff (ref :: REF | e) Boolean
test_Move = do
  chip <- newChip
  setReg chip (reg 4) (byte 0x42)
  eval chip (Move (reg 3) (reg 4))
  getReg chip (reg 3) `shouldEql` byte 0x42

test_BitAnd :: forall e. Eff (ref :: REF | e) Boolean
test_BitAnd = do
  chip <- newChip
  setReg chip (reg 4) (byte 0x42)
  setReg chip (reg 5) (byte 0x2)  
  eval chip (BitAnd (reg 4) (reg 5))
  getReg chip (reg 4) `shouldEql` byte 0x2

test_BitXor :: forall e. Eff (ref :: REF | e) Boolean
test_BitXor = do
  chip <- newChip
  setReg chip (reg 4) (byte 0x32)
  setReg chip (reg 5) (byte 0x42)  
  eval chip (BitXor (reg 4) (reg 5))
  getReg chip (reg 4) `shouldEql` byte 0x70

test_Add_noOverflow :: forall e. Eff (ref :: REF | e) Boolean
test_Add_noOverflow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0x42)
  setReg chip (reg 8) (byte 0x2)  
  eval chip (Add (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0x44) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x0)

test_Add_overflow :: forall e. Eff (ref :: REF | e) Boolean
test_Add_overflow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0xff)
  setReg chip (reg 8) (byte 0x3)  
  eval chip (Add (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0x2) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x1)

test_Sub_noBorrow :: forall e. Eff (ref :: REF | e) Boolean
test_Sub_noBorrow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0x42)
  setReg chip (reg 8) (byte 0x2)  
  eval chip (Sub (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0x40) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x1)

test_Sub_borrow :: forall e. Eff (ref :: REF | e) Boolean
test_Sub_borrow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0x3)
  setReg chip (reg 8) (byte 0xff)  
  eval chip (Sub (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0x4) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x0)

test_BitShiftR :: forall e. Eff (ref :: REF | e) Boolean
test_BitShiftR = do
  chip <- newChip
  setReg chip (reg 5) (byte 0x3)
  eval chip (BitShiftR (reg 5))
  (getReg chip (reg 5) `shouldEql` byte 0x1) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x1)

test_SubFlip_borrow :: forall e. Eff (ref :: REF | e) Boolean
test_SubFlip_borrow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0x42)
  setReg chip (reg 8) (byte 0x2)  
  eval chip (SubFlip (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0xC0) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x0)

test_SubFlip_noBorrow :: forall e. Eff (ref :: REF | e) Boolean
test_SubFlip_noBorrow = do
  chip <- newChip
  setReg chip (reg 7) (byte 0x3)
  setReg chip (reg 8) (byte 0xff)  
  eval chip (SubFlip (reg 7) (reg 8))
  (getReg chip (reg 7) `shouldEql` byte 0xfc) `and`
  (getReg chip (reg 0xf) `shouldEql` byte 0x1)

test_SkipIfNotEq :: forall e. Eff (ref :: REF | e) Boolean
test_SkipIfNotEq = do
  chip <- newChip
  pc <- getPc chip
  setReg chip (reg 4) (byte 0x42)
  setReg chip (reg 3) (byte 0x40)  
  eval chip (SkipIfNotEq (reg 3) (reg 4))
  getPc chip `shouldEql` (pc `plus` 2)

test_MovePtr :: forall e. Eff (ref :: REF | e) Boolean
test_MovePtr = do
  chip <- newChip
  eval chip (MovePtr (addr 0x123))
  getPtr chip `shouldEql` addr 0x123

test_AddToPtr :: forall e. Eff (ref :: REF | e) Boolean
test_AddToPtr = do
  chip <- newChip
  setPtr chip (addr 0x100)
  setReg chip (reg 7) (byte 0x10)
  eval chip (AddToPtr (reg 7))
  getPtr chip `shouldEql` addr 0x110

test_SetCharAddr :: forall e. Eff (ref :: REF | e) Boolean
test_SetCharAddr = do
  chip <- newChip
  setReg chip (reg 8) (byte 0xa)
  eval chip (SetCharAddr (reg 8))
  getPtr chip `shouldEql` addr (0xa * 5)

test_StoreBcd :: forall e. Eff (ref :: REF | e) Boolean
test_StoreBcd = do
  chip <- newChip
  setPtr chip (addr 0x100)
  setReg chip (reg 5) (byte 24)
  eval chip (StoreBcd (reg 5))
  dumpBytes chip `shouldEql` (byte <$> [0, 2, 4])
  where
    dumpBytes chip = readMem chip (addr 0x100) (byte 3)
  
test_DumpRegs :: forall e. Eff (ref :: REF | e) Boolean
test_DumpRegs = do
  chip <- newChip
  setPtr chip (addr 0x100)
  setReg chip (reg 0) (byte 0x4)
  setReg chip (reg 1) (byte 0x5)
  setReg chip (reg 2) (byte 0x6)
  eval chip (DumpRegs (reg 2))
  ((dumpBytes chip) `shouldEql` (byte <$> [0x4, 0x5, 0x6])) `and`
  (getPtr chip `shouldEql` (addr 0x103))
  where
    dumpBytes chip = readMem chip (addr 0x100) (byte 3)

test_LoadRegs :: forall e. Eff (ref :: REF | e) Boolean
test_LoadRegs = do
  chip <- newChip
  let values = (byte <$> [10, 11, 12, 13])
  setPtr chip (addr 0x100)
  writeMem chip (addr 0x100) values
  eval chip (LoadRegs (reg 3))
  (dumpRegs chip `shouldEql` values) `and`
  (getPtr chip `shouldEql` addr 0x104)
  where
    dumpRegs chip = slice chip.regs 0 4 
      

evalTests :: forall e. TestSuite (TestEffects e)
evalTests = do
  checks "Return" test_Return      
  checks "Jump" test_Jump    
  checks "Call" test_Call      
  checks "SkipIfEqImm" test_SkipIfEqImm
  checks "SkipIfNotEqImm" test_SkipIfNotEqImm
  checks "MoveImm" test_MoveImm
  checks "AddImm" test_AddImm
  checks "AddImm_withOverflow" test_AddImm_withOverflow    
  checks "Move" test_Move
  checks "BitAnd" test_BitAnd
  checks "BitXor" test_BitXor  
  checks "Add" test_Add_noOverflow
  checks "Add" test_Add_overflow
  checks "Sub" test_Sub_noBorrow
  checks "Sub" test_Sub_borrow
  checks "BitShiftR" test_BitShiftR
  checks "SubFlip" test_SubFlip_noBorrow
  checks "SubFlip" test_SubFlip_borrow
  checks "SkipIfNotEq" test_SkipIfNotEq  
  checks "MovePtr" test_MovePtr
  checks "AddToPtr" test_AddToPtr
  checks "SetCharAddr" test_SetCharAddr  
  checks "StoreBcd" test_StoreBcd
  checks "DumpRegs" test_DumpRegs
  checks "LoadRegs" test_LoadRegs  
  
checks name tester = do
  test name do
    result <- liftEff tester
    Assert.equal true result


main :: forall e. Eff (TestEffects e) Unit
main = runTest do
  suite "Eval tests" (evalTests)
