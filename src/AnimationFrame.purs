module AnimationFrame ( DOM
                      , requestAnimationFrame
                      ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)

foreign import data DOM :: Effect

foreign import requestAnimationFrame
  :: forall e a
   . Eff (dom :: DOM | e) a
  -> Eff (dom :: DOM | e) Unit
