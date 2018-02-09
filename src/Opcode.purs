module Opcode ( Opcode(..)
              , decode
              , isIoInstruction
              ) where

import Prelude

import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple4, (/\))
import Partial.Unsafe (unsafeCrashWith)
import Types (Addr, Byte, Reg, byte, pack, reg, unpack)

data Opcode
  = Eof
  | ClearDisplay
  | Return
  | Jump Addr
  | Call Addr
  | SkipIfEqImm Reg Byte
  | SkipIfNotEqImm Reg Byte
  | SkipIfEq Reg Reg
  | MoveImm Reg Byte
  | AddImm Reg Byte
  | Move Reg Reg
  | BitAnd Reg Reg
  | BitXor Reg Reg    
  | Add Reg Reg
  | Sub Reg Reg
  | BitShiftR Reg
  | SubFlip Reg Reg    
  | SkipIfNotEq Reg Reg        
  | MovePtr Addr
  | SetRandom Reg Byte
  | DrawSprite Reg Reg Byte
  | SkipIfKey Reg            
  | SkipIfNotKey Reg
  | GetTimer Reg
  | GetKey Reg            
  | StartTimer Reg
  | PlaySound Reg        
  | AddToPtr Reg
  | SetCharAddr Reg        
  | StoreBcd Reg    
  | DumpRegs Reg
  | LoadRegs Reg    


instance showOpcode :: Show Opcode where
  show (Eof) = "Eof"
  show (ClearDisplay) = "ClearDisplay"  
  show (Return) = "Return"
  show (Jump a) = "Jump " <> show a
  show (Call a) = "Call " <> show a  
  show (SkipIfEqImm r val) = "SkipIfEqImm " <> show r <> " " <> show val
  show (SkipIfNotEqImm r val) = "SkipIfNotEqImm " <> show r <> " " <> show val
  show (SkipIfEq r1 r2) = "SkipIfEq " <> show r1 <> " " <> show r2
  show (MoveImm r val) = "MoveImm " <> show r <> " " <> show val
  show (AddImm r val) = "AddImm " <> show r <> " " <> show val
  show (Move r1 r2) = "Move " <> show r1 <> " " <> show r2
  show (BitAnd r1 r2) = "BitAnd " <> show r1 <> " " <> show r2
  show (BitXor r1 r2) = "BitXor " <> show r1 <> " " <> show r2  
  show (Add r1 r2) = "Add " <> show r1 <> " " <> show r2
  show (Sub r1 r2) = "Sub " <> show r1 <> " " <> show r2
  show (BitShiftR r) = "BitShiftR " <> show r
  show (SubFlip r1 r2) = "SubFlip " <> show r1 <> " " <> show r2  
  show (SkipIfNotEq r1 r2) = "SkipIfNotEq " <> show r1 <> " " <> show r2  
  show (MovePtr a) = "MovePtr " <> show a
  show (SetRandom r val) = "SetRandom " <> show r <> " " <> show val
  show (DrawSprite r1 r2 h) =
    "DrawSprite " <> show r1  <> " " <> show r2 <> " " <> show h
  show (SkipIfKey r) = "SkipIfKey " <> show r    
  show (SkipIfNotKey r) = "SkipIfNotKey " <> show r
  show (GetTimer r) = "GetTimer " <> show r
  show (GetKey r) = "GetKey " <> show r  
  show (StartTimer r) = "StartTimer " <> show r
  show (PlaySound r) = "PlaySound " <> show r  
  show (AddToPtr r) = "AddToPtr " <> show r
  show (SetCharAddr r) = "SetCharAddr " <> show r
  show (StoreBcd r) = "StoreBcd " <> show r
  show (DumpRegs r) = "DumpRegs " <> show r
  show (LoadRegs r) = "LoadRegs " <> show r  

decode :: Tuple Byte Byte -> Opcode
decode (Tuple b1 b2) =
  case n1, n2, n3, n4 of
    0x0, 0x0, 0x0, 0x0 -> Eof
    0x0, 0x0, 0xE, 0x0 -> ClearDisplay
    0x0, 0x0, 0xE, 0xE -> Return
    0x1, _, _, _ -> Jump addr
    0x2, _, _, _ -> Call addr
    0x3, x, _, _ -> SkipIfEqImm (reg x) val
    0x4, x, _, _ -> SkipIfNotEqImm (reg x) val
    0x5, x, y, _ -> SkipIfEq (reg x) (reg y)
    0x6, x, _, _ -> MoveImm (reg x) val
    0x7, x, _, _ -> AddImm (reg x) val
    0x8, x, y, 0x0 -> Move (reg x) (reg y)
    0x8, x, y, 0x2 -> BitAnd (reg x) (reg y)
    0x8, x, y, 0x3 -> BitXor (reg x) (reg y)
    0x8, x, y, 0x4 -> Add (reg x) (reg y)
    0x8, x, y, 0x5 -> Sub (reg x) (reg y)
    0x8, x, 0x0, 0x6 -> BitShiftR (reg x)
    0x8, x, y,0x7 -> SubFlip (reg x) (reg y)
    0x9, x, y, 0x0 -> SkipIfNotEq (reg x) (reg y)
    0xA, _, _, _ -> MovePtr addr
    0xC, x, _, _ -> SetRandom (reg x) val
    0xD, x, y, h -> DrawSprite (reg x) (reg y) (byte h)
    0xE, x, 0x9, 0xE -> SkipIfKey (reg x)
    0xE, x, 0xA, 0x1 -> SkipIfNotKey (reg x)
    0xF, x, 0x0, 0x7 -> GetTimer (reg x)
    0xf, x, 0x0, 0xA -> GetKey (reg x)
    0xF, x, 0x1, 0x5 -> StartTimer (reg x)
    0xF, x, 0x1, 0x8 -> PlaySound (reg x)
    0xF, x, 0x1, 0xE -> AddToPtr (reg x)
    0xF, x, 0x2, 0x9 -> SetCharAddr (reg x)
    0xF, x, 0x3, 0x3 -> StoreBcd (reg x)
    0xF, x, 0x5, 0x5 -> DumpRegs (reg x)
    0xF, x, 0x6, 0x5 -> LoadRegs (reg x)
    _, _, _, _ -> unsafeCrashWith ("decode: unhandled code - " <> word)
  where
    tuple4' [a, b, c, d] = tuple4 a b c d
    tuple4' _ = unsafeCrashWith "bad unpack"

    n1 /\ n2 /\ n3 /\ n4 /\ unit = tuple4' (unpack b1 <> unpack b2)
    val = pack [n3, n4]
    addr = pack [n2, n3, n4]
      
    word = "0x" <> foldl (<>) "" (show <$> [b1, b2])


isIoInstruction :: Opcode -> Boolean
isIoInstruction =
  case _ of
    ClearDisplay -> true
    SetRandom _ _ -> true
    DrawSprite _ _ _ -> true
    GetKey _ -> true
    StartTimer _ -> true
    PlaySound _ -> true
    _ -> false
