{-# LANGUAGE GADTs #-}
-- | Intermedite representation
-- The plan is to convert every language we support to the intermediate
-- representation language.  From there we utilize hoopl to create
-- control flow and data flow graphs.
--
-- TODO:
--   * the IR AST is currently way to minimal to represent any kind of
--   input language
--   * the annotation is tacked on to the IR AST. It feels like a crazy
--   mistake, but the only way to do this at this point is to run with
--   it and find out
--
module IR where
import Compiler.Hoopl


-- | An expression is a bunch of variables
-- TODO, this will have to be extended to be a proper data type
--
type Expr = Var

-- | A variable has a name and a source location
--
data Var = Variable { id :: String, loc :: SrcLocation }
    deriving Show

-- | A source location provides information about the where abouts in
-- the source code
--
data SrcLocation = SingleLocation { row :: Int, column :: Int }
              | CoLinearLocation { row :: Int, startColumn :: Int, endColumn :: Int }
              | MultiLineLocation { startRow :: Int, startColumn :: Int, endRow :: Int, endColumn :: Int }

instance Show SrcLocation where
    show (SingleLocation r c) = concat ["R: ", show r, " C: ",  show c]
    show (CoLinearLocation r sc ec) = concat ["R: ", show r, " C: ", show sc, "-", show ec]
    show (MultiLineLocation sr sc er ec) = concat ["R: ", show sr, "-", show er, "C: ", show sc, "-", show ec]


-- | A procedure in an AST
--
data Proc = Proc { name :: String, args :: [Var], entry :: Label, body :: Graph (Insn SrcLocation) C C }

instance Show (Proc) where
    show (Proc {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g

-- | An instruction with an (e)ntry and an e(x)it
--
data Insn a e x where
    Label   :: Label    ->              Insn a C O
    Assign  :: Var      -> Expr   ->    Insn a O O
    Return  :: Maybe [Expr]   ->        Insn a O C

instance NonLocal (Insn a) where
    entryLabel (Label l)    = l
    successors (Return _)   = []

instance Show (Insn a e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Assign v e) =  show v ++ " = " ++ show e
    show (Return (Just xs)) = show $ fmap show xs
    show (Return Nothing) = ""
