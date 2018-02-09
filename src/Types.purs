module Types ( class IntOperations
             , class Packed
             , Byte
             , Addr
             , Reg
             , byte
             , addr
             , reg
             , int
             , plus
             , minus
             , strToBytes
             , unpack
             , pack
             , bits
             , and
             , xor
             , shr
             , bcd ) where

import Prelude

import Data.Array ((:))
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits as B
import Data.String (length, toCharArray)
import Partial.Unsafe (unsafeCrashWith)

newtype Byte = B Int

newtype Addr = A Int

newtype Reg = R Int

byte :: Int -> Byte
byte x | x >= 0 && x <= 255 = B x
byte x | x > 255 = B (x `mod` 256)
byte x | x < 0   = B (x `mod` 256 + 256)
byte _ = unsafeCrashWith "invalid case"

addr :: Int -> Addr
addr x | x >= 0 && x <= 4095 = A x
addr x = unsafeCrashWith ("invalid addr " <> show x)

reg :: Int -> Reg
reg x | x >= 0 && x <= 15 = R x
reg x = unsafeCrashWith ("invalid register " <> show x)

toHex :: Int -> String
toHex = toStringAs hexadecimal

strToBytes :: String -> Array Byte
strToBytes str = (byte <<< toCharCode) <$> toCharArray str

and :: Byte -> Byte -> Byte
and (B x) (B y) = byte (x `B.and` y)

xor :: Byte -> Byte -> Byte
xor (B x) (B y) = byte (x `B.xor` y)

shr :: Byte -> Int -> Byte
shr (B x) n = byte (x `B.shr` n)

bcd :: Byte -> Array Byte
bcd val = byte <$> [ (num `div` 100) `mod` 10
                   , (num `div` 10)  `mod` 10
                   , (num `div` 1)   `mod` 10
                   ]
    where num = int val


derive instance eqByte :: Eq Byte

derive instance ordByte :: Ord Byte

derive instance eqAddr :: Eq Addr

derive instance eqReg :: Eq Reg

class IntOperations a where
  int :: a -> Int
  plus :: a -> Int -> a
  minus :: a -> Int -> a

instance intOperationsByte :: IntOperations Byte where
  int (B x) = x
  plus (B x) y = byte (x + y)
  minus (B x) y = byte (x - y)  

instance intOperationsAddr :: IntOperations Addr where
  int (A x) = x
  plus (A x) y = addr (x + y)
  minus (A x) y = addr (x - y)  

instance intOperationsReg :: IntOperations Reg where
  int (R x) = x
  plus (R x) y = reg (x + y)
  minus (R x) y = reg (x - y)

class Packed a where
  unpack :: a -> Array Int
  pack :: Array Int -> a
  bits :: a -> Array Int

instance packedByte :: Packed Byte where
  unpack (B x) = extractNibbles 2 x
  bits (B x) = extractBits 8 x
  pack = byte <<< packInts

instance packedAddr :: Packed Addr where
  unpack (A x) = extractNibbles 3 x
  bits (A x) = extractBits 8 x
  pack = addr <<< packInts

packInts :: Array Int -> Int
packInts xs = foldl (\acc x -> (acc `B.shl` 4) + x) 0 xs

extractNibbles :: Int -> Int -> Array Int
extractNibbles 0 x = []
extractNibbles n x =
    left : extractNibbles (n - 1) x
    where 
      left = (x `B.shr` ((n - 1) * 4)) `B.and` 0xf

extractBits :: Int -> Int -> Array Int
extractBits 0 x = []
extractBits n x =
    left : extractBits (n - 1) x
    where
      left = (x `B.shr` (n - 1)) `B.and` 0x1

instance showByte :: Show Byte where
  show (B x) =
    if (length hex == 2) then hex else ("0" <> hex)
    where
      hex = toHex x


instance showAddr :: Show Addr where
  show (A x) = "0x" <> toHex x

instance showRegister :: Show Reg where
  show (R x) = "V" <> toHex x

instance byteSemiring :: Semiring Byte where
  add (B x) (B y) = byte (x + y)
  zero = B 0
  mul (B x) (B y) = byte (x * y)
  one = B 1

instance ringByte :: Ring Byte where
  sub (B x) (B y) = if x > y
                       then byte (x - y)
                       else byte (x - y + 256)
