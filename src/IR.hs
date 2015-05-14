{-# LANGUAGE GADTs #-}
module IR where

-- with the help from:
-- http://blog.ezyang.com/2011/04/hoopl-guided-tour-base-system/
--
import Compiler.Hoopl


data Expr = Var Var
    deriving Show

type Var = String

-- | A procedure in an AST
--
data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph Insn C C }

-- | An instruction with an (e)ntry and an e(x)it
--
data Insn e x where
    Label   :: Label    ->              Insn C O
    Assign  :: Var      -> Expr ->      Insn O O
    Return  :: Maybe [Expr]   ->        Insn O C


instance NonLocal Insn where
    entryLabel (Label l)    = l
    successors (Return _)   = []

instance Show (Proc) where
    show (Proc {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g

instance Show (Insn e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Assign v e) =  show v ++ " = " ++ show e
    show (Return (Just xs)) = show $ fmap show xs
    show (Return Nothing) = ""
