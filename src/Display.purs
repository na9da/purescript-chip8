module Display ( Display
               , newDisplay
               , drawSprite
               , clearDisplay
               ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array (length, unsafeIndex, (..))
import Data.ArrayBuffer.ArrayBuffer (ARRAY_BUFFER)
import Data.ArrayBuffer.Typed (unsafeAt)
import Data.Foldable (foldM)
import Data.Int (toNumber)
import Graphics.Canvas (CANVAS, Context2D, ScaleTransform, fillRect, getImageData, imageDataBuffer, scale, setFillStyle)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Control.Monad.Eff.Console (CONSOLE, logShow)

data Color
    = Black
    | White

derive instance eqColor :: Eq Color

toHex :: Color -> String
toHex = case _ of
  Black -> "#000000"
  White -> "#FFFFFF"


type Display = { ctx :: Context2D
               , scale :: ScaleTransform
               }

newDisplay
  :: forall e
   . Context2D
  -> ScaleTransform
  -> Eff (canvas :: CANVAS | e) Display
newDisplay ctx scaleVal = do
  void $ scale scaleVal ctx
  let display = { ctx: ctx
                , scale: scaleVal
                }
  clearDisplay display
  pure display

clearDisplay :: forall e. Display -> Eff (canvas :: CANVAS | e) Unit
clearDisplay {ctx} = do
  void $ setFillStyle "#000000" ctx
  void $ fillRect ctx {x: 0.0, y: 0.0, w: 64.0, h: 32.0}  

getColor
  :: forall e
   . Display
  -> Number
  -> Number
  -> Eff (arrayBuffer :: ARRAY_BUFFER, canvas :: CANVAS, console :: CONSOLE | e) Color
getColor {ctx, scale} x y = do
  pix <- getPixel
  r <- unsafeAt pix 0
  g <- unsafeAt pix 1
  b <- unsafeAt pix 2
  if r + g + b == 0.0
     then pure Black
     else pure White
  where
    getPixel = do
      img <- getImageData ctx (x * scale.scaleX) (y * scale.scaleY) 1.0 1.0
      pure (imageDataBuffer img)

setColor
  :: forall e
   . Display
  -> Number
  -> Number
  -> Color
  -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
setColor {ctx} x y color = do
  void $ setFillStyle (toHex color) ctx
  void $ fillRect ctx {x: x, y: y, w: 1.0, h: 1.0}


togglePixel
  :: forall e
   . Display
  -> Number
  -> Number
  -> Eff (arrayBuffer :: ARRAY_BUFFER, canvas :: CANVAS, console :: CONSOLE | e) Boolean
togglePixel display x y = do
  color <- getColor display x y
  if color == Black
     then setColor display x y White *> pure false
     else setColor display x y Black *> pure true

drawSprite
  :: forall e
   . Display
  -> Number
  -> Number
  -> (Array (Array Int))
  -> Eff (arrayBuffer :: ARRAY_BUFFER, canvas :: CANVAS, console :: CONSOLE | e) Boolean
drawSprite display x y sprite = do
  foldM or false (forEachBit togglePixel' sprite)
  where
    or :: forall m. Monad m => Boolean -> m Boolean -> m Boolean
    or a m = pure ((||) a) <*> m
    
    togglePixel' i j 0 = pure false
    togglePixel' i j 1 = togglePixel display (x + toNumber i) (y + toNumber j)
    togglePixel' _ _ _ = unsafeCrashWith "invalid case"

    forEachBit f array = do
      j <- 0 .. (length array - 1)
      let row = unsafePartial (unsafeIndex sprite j)
      i <- 0 .. (length row - 1)
      let bit = unsafePartial (unsafeIndex row i)
      pure (f i j bit)

