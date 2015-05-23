{-# LANGUAGE GADTs #-}
module Python.Hoopl where

import qualified Language.Python.Common.AST as PA
import Compiler.Hoopl
import Language.Python.Common.SrcLocation


data Insn a e x where
    Label   :: Label -> Insn a C O
    Assign  :: [PA.Expr SrcSpan] -> PA.Expr SrcSpan -> SrcSpan -> Insn a O O
    While   :: PA.Expr SrcSpan -> PA.Suite SrcSpan -> PA.Suite SrcSpan -> SrcSpan -> Insn a O O
    Return  :: Maybe (PA.Expr SrcSpan) -> SrcSpan -> Insn a O C

data Proc = Proc { name :: String
                 , args :: [PA.Parameter SrcSpan]
                 , entry :: Label
                 , body :: Graph (Insn SrcSpan) C C }

instance NonLocal (Insn a) where
    entryLabel (Label l)    = l
    successors (Return _ _)   = []


instance Show (Proc) where
    show (Proc {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g


instance Show (Insn a e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Assign v e _) =  show v ++ " = " ++ show e
    show (While c b e _) = show c ++ show b ++ show e
    show (Return (Just xs) _) = show xs
    show (Return Nothing _) = ""
