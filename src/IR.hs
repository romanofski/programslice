{-# LANGUAGE GADTs #-}
module IR where

-- with the help from:
-- http://blog.ezyang.com/2011/04/hoopl-guided-tour-base-system/
--
import Compiler.Hoopl


-- | Make sure when converting Strings to (Hoopl) Labels, we avoid any
-- duplication. The same strings will be assigned to the same Label
--
type M = CheckingFuelMonad SimpleUniqueMonad

data Expr = Var Var

type Var = String

-- | A procedure in an AST
--
data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph Insn C C }

-- | An instruction with an (e)ntry and an e(x)it
--
data Insn e x where
    Label   :: Label    ->              Insn C O
    Assign  :: Var      -> Expr ->      Insn O O
    Return  :: Maybe [Expr]   ->              Insn O C


instance NonLocal Insn where
    entryLabel (Label l)    = l
    successors (Return _)   = []
