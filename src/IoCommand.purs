module IoCommand ( IoCommand(..)
                 ) where

import Opcode (Opcode)

data IoCommand
    = Next
    | Exit
    | Io Opcode
