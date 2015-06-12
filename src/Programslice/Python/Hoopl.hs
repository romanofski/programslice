{-# LANGUAGE GADTs #-}
module Programslice.Python.Hoopl where

import qualified Language.Python.Common.AST as PA
import Compiler.Hoopl
import Language.Python.Common.SrcLocation


data Insn e x where
    Label   :: Label -> Insn C O
    Normal  :: PA.Statement SrcSpan -> Label -> Insn O O
    Return  :: Maybe (PA.Expr SrcSpan) -> Insn O C


-- | A representation of a control flow graph
--
-- Note: The current intention of constructing control flow graphs is
-- on a function level, until I can utilise Hoopl better.
--
data CFG = CFG { name :: String                     -- ^ function name
                , args :: [PA.Parameter SrcSpan]    -- ^ function parameters
                , entry :: Label                    -- ^ function entry label
                , body :: Graph Insn C C            -- ^ CFG of the function
                }


instance NonLocal (Insn) where
    entryLabel (Label l)    = l
    successors (Return _ )   = []


instance Show (CFG) where
    show (CFG {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g


instance Show (Insn e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Normal _ lbl) = show lbl
    show (Return (Just xs)) = show xs
    show (Return Nothing) = ""
