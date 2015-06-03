{-# LANGUAGE GADTs #-}
module Python.Hoopl where

import qualified Language.Python.Common.AST as PA
import Compiler.Hoopl
import Language.Python.Common.SrcLocation


data Insn e x where
    Label   :: Label -> Insn C O
    Normal  :: PA.Statement SrcSpan -> Label -> Insn O O
    Return  :: Maybe (PA.Expr SrcSpan) -> Insn O C

data Proc = Proc { name :: String
                 , args :: [PA.Parameter SrcSpan]
                 , entry :: Label
                 , body :: Graph Insn C C }

instance NonLocal (Insn) where
    entryLabel (Label l)    = l
    successors (Return _ )   = []


instance Show (Proc) where
    show (Proc {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g


instance Show (Insn e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Normal _ lbl) = show lbl
    show (Return (Just xs)) = show xs
    show (Return Nothing) = ""
