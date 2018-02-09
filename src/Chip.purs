module Chip ( Chip
            , showChip
            , fetch
            , eval
            , getPc
            , getPtr
            , setPtr
            , getReg
            , setReg
            , newChip
            , loadRom
            , readMem
            , writeMem
            , pushStack
            , popStack
            ) where

import Prelude
import RefArray

import Charset (defaultCharset, loadCharset, lookupChar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Data.Array as A
import Data.Tuple (Tuple(..))
import IoCommand (IoCommand(..))
import Opcode (Opcode(..), isIoInstruction)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Types (Addr, Byte, Reg, addr, and, bcd, byte, int, plus, reg, shr, xor)

type Chip = { pc :: Ref Addr
            , ptr :: Ref Addr
            , regs :: RefArray Byte
            , mem :: RefArray Byte
            , stack :: RefArray Addr
            , timer :: Ref Byte
            , keys :: RefArray Boolean
            }

newChip :: forall e. Eff (ref :: REF | e) Chip
newChip = do
  pc <- newRef (addr 0x200)
  ptr <- newRef (addr 0x000)
  regs <- newRefArray (A.replicate 16 (byte 0))
  stack <- newRefArray []
  timer <- newRef (byte 0)
  keys <- newRefArray (A.replicate 16 false)
  mem <- newRefArray (A.replicate 4096 (byte 0))
  loadCharset mem defaultCharset
  pure { pc: pc
       , ptr: ptr
       , regs: regs
       , mem: mem
       , stack: stack
       , timer: timer
       , keys: keys
       }

showChip :: forall eff. Chip -> Eff (ref :: REF | eff) String
showChip chip = do
  pc <- show <$> readRef chip.pc
  ptr <- show <$> readRef chip.ptr
  timer <- show <$> readRef chip.timer
  regs <- show <$> getRefArray chip.regs
  stack <- show <$> getRefArray chip.stack
  keys <- show <$> getRefArray chip.mem  
  mem <- show <$> getRefArray chip.mem
  pure $ "Chip {"
           <> "pc: " <> pc
           <> ", ptr: " <> ptr
           <> ", timer: " <> timer
           <> ", regs: " <> regs
           <> ", stack: " <> stack
           <> ", keys: " <> keys
           <> ", mem: " <> mem
           <> "}"

getPc :: forall e. Chip -> Eff (ref :: REF | e) Addr
getPc {pc} = readRef pc

setPc :: forall e. Chip -> Addr -> Eff (ref :: REF | e) Unit
setPc {pc} = writeRef pc

getPtr :: forall e. Chip -> Eff (ref :: REF | e) Addr
getPtr {ptr} = readRef ptr

setPtr :: forall e. Chip -> Addr -> Eff (ref :: REF | e) Unit
setPtr {ptr} = writeRef ptr

getReg :: forall e. Chip -> Reg -> Eff (ref :: REF | e) Byte
getReg {regs} r = index regs (int r)

setReg :: forall e. Chip -> Reg -> Byte -> Eff (ref :: REF | e) Unit
setReg {regs} r = updateAt regs (int r) 

loadRom :: forall e. Chip -> Array Byte -> Eff (ref :: REF | e) Unit
loadRom {mem} bytes = splice mem 0x200 (A.length bytes) bytes

readMem :: forall e. Chip -> Addr -> Byte -> Eff (ref :: REF | e) (Array Byte)
readMem {mem} addr len = slice mem (int addr) (int len)

writeMem :: forall e. Chip -> Addr -> Array Byte -> Eff (ref :: REF | e) Unit
writeMem {mem} addr bytes = splice mem (int addr) (A.length bytes)  bytes

pushStack :: forall e. Chip -> Addr -> Eff (ref :: REF | e) Unit
pushStack {stack} = push stack

popStack :: forall e. Chip -> Eff (ref :: REF | e) Addr
popStack {stack} = pop stack

skip :: forall e. Chip ->  Eff (ref :: REF | e) Unit
skip {pc: ref} = do
  pc <- readRef ref
  writeRef ref (pc `plus` 2)

fetch :: forall e. Chip -> Eff (ref :: REF | e) (Tuple Byte Byte)
fetch chip@{mem} = unsafePartial do
  addr <- getPc chip
  skip chip
  [b1, b2] <- slice mem (int addr) 2
  pure (Tuple b1 b2)

eval :: forall e. Chip -> Opcode -> Eff (ref :: REF | e) IoCommand
eval chip Eof = pure Exit
eval chip op | isIoInstruction op = pure (Io op)
eval chip opcode = do
  case opcode of
    Return -> popStack chip >>= setPc chip
    Call addr -> do
      getPc chip >>= pushStack chip
      setPc chip addr
    Jump addr ->
      writeRef chip.pc addr
    SkipIfEqImm r imm -> do
      val <- getReg chip r
      if val == imm
         then skip chip
         else pure unit
    SkipIfNotEqImm r imm -> do
      val <- getReg chip r
      if val /= imm
         then skip chip
         else pure unit
    SkipIfEq r1 r2 -> do
      val1 <- getReg chip r1
      val2 <- getReg chip r2
      if val1 == val2
        then skip chip
        else pure unit
    MoveImm r val -> setReg chip r val
    AddImm r val -> do
      adder <- getReg chip r
      setReg chip r (adder + val)
    Move r1 r2 -> getReg chip r2 >>= setReg chip r1
    BitAnd r1 r2 -> do
      v1 <- getReg chip r1
      v2 <- getReg chip r2
      setReg chip r1 (v1 `and` v2)
    BitXor r1 r2 -> do
      v1 <- getReg chip r1
      v2 <- getReg chip r2
      setReg chip r1 (v1 `xor` v2)
    Add r1 r2 -> do
      v1 <- getReg chip r1
      v2 <- getReg chip r2
      let sum = int v1 + int v2
      if sum > 0xff
         then do -- carry
           setReg chip r1 (v1 + v2)
           setReg chip (reg 0xf) (byte 0x1)
         else do -- no carry
           setReg chip r1 (v1 + v2)
           setReg chip (reg 0xf) (byte 0x0)
    Sub r1 r2 -> do
      v1 <- getReg chip r1
      v2 <- getReg chip r2
      if v1 >= v2
         then do -- no borrow
           setReg chip r1 (v1 - v2)
           setReg chip (reg 0xf) (byte 0x1)
         else do -- borrow
           setReg chip r1 (v1 - v2)
           setReg chip (reg 0xf) (byte 0x0)
    BitShiftR r -> do
      val <- getReg chip r
      setReg chip r (val `shr` 1)
      setReg chip (reg 0xf) (val `and` byte 0x1)
    SubFlip r1 r2 -> do
      v1 <- getReg chip r1
      v2 <- getReg chip r2
      if v2 >= v1
         then do -- no borrow
           setReg chip r1 (v2 - v1)
           setReg chip (reg 0xf) (byte 0x1)
         else do -- borrow
           setReg chip r1 (v2 - v1)
           setReg chip (reg 0xf) (byte 0x0)
    SkipIfNotEq r1 r2 -> do
      val1 <- getReg chip r1
      val2 <- getReg chip r2
      if val1 /= val2
         then skip chip
         else pure unit
    MovePtr addr -> setPtr chip addr
    SkipIfKey r -> do
      key <- getReg chip r
      pressed <- index chip.keys (int key)
      if pressed
         then skip chip
         else pure unit
    SkipIfNotKey r -> do
      key <- getReg chip r
      pressed <- index chip.keys (int key)
      if not pressed
         then skip chip
         else pure unit
    GetTimer r -> readRef chip.timer >>= setReg chip r
    AddToPtr r -> do
      addr <- getPtr chip
      val <- getReg chip r
      setPtr chip (addr `plus` int val)
    SetCharAddr r -> do
      char <- getReg chip r
      setPtr chip (lookupChar char)
    StoreBcd r -> do
      digits <- bcd <$> getReg chip r
      addr <- getPtr chip
      writeMem chip addr digits
    DumpRegs r -> do
      addr <- getPtr chip
      let count = int r + 1
      bytes <- slice chip.regs 0 count
      writeMem chip addr bytes
      setPtr chip (addr `plus` count)
    LoadRegs r -> do
      addr <- getPtr chip
      let count = int r + 1
      values <- readMem chip addr (byte count)
      splice chip.regs 0 count values
      setPtr chip (addr `plus` count)
    _ -> unsafeCrashWith ("eval: unhandled opcode - " <> show opcode)
  pure Next

