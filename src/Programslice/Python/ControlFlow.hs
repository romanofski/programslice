{-# LANGUAGE RankNTypes, GADTs #-}
module Programslice.Python.ControlFlow where

import Compiler.Hoopl
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST()
import Control.Monad.State
import Data.Tuple (swap)
import qualified Data.Map as M


-- | Map to avoid creating new labels for already known identifiers
--
type IdLabelMap = M.Map (Statement SrcSpan) Label
type LabelBlockMap = M.Map Label (Statement SrcSpan)

-- | A representation of a control flow graph
--
-- Note: The current intention of constructing control flow graphs is
-- on a function level, until I can utilise Hoopl better.
--
data CFG = CFG { name :: String                     -- ^ function name
                , args :: [Parameter SrcSpan]       -- ^ function parameters
                , cfgEntryLabel :: Label            -- ^ function entry label
                , cfgBody :: Graph Insn C C            -- ^ CFG of the function
                , blockLabelMap :: IdLabelMap            -- ^ maps source code blocks to labels
                , labelBlockMap :: LabelBlockMap         -- ^ maps labels to code blocks
                }


-- | Monadic environment making sure that we generate a fresh unique
-- label for basic blocks or return a label if we've seen the basic
-- block.
--
type CFGBuilder a = StateT IdLabelMap SimpleUniqueMonad a

instance Show (CFG) where
    show (CFG {name = n, cfgEntryLabel = lbl, cfgBody = g }) =
        show $ show lbl ++ "\n" ++ n ++ ":" ++ graph
        where graph = showGraph prettyText g

-- | A control flow instruction
--
data Insn e x where
    Label   :: Label -> Insn C O
    Normal  :: Statement SrcSpan -> Insn O O
    Exit    :: Statement SrcSpan -> Insn O C

-- | Nonlocal control flow
--
instance NonLocal (Insn) where
    entryLabel (Label l)   = l
    successors (Exit  _)   = []

instance Show (Insn e x) where
    show (Label lbl)  = "[CO]" ++ show lbl ++ ":"
    show (Exit stm) = "[OC]" ++ show stm
    show _ = "--"

instance Pretty (Insn e x) where
    pretty (Normal stm) = pretty stm
    pretty (Exit stm) = pretty stm
    pretty _ = text ""

-- | Creates a new label for the given string or source code block.
-- If we have already created a label return the given label, otherwise
-- create a new one and return it.
--
labelFor :: Statement SrcSpan -> CFGBuilder Label
labelFor srcfrag = do
    m <- get
    case M.lookup srcfrag m of
            Just l -> return l
            Nothing -> do
                l <- lift freshLabel
                put $ M.insert srcfrag l m
                return l


-- Control Flow Graph functions

-- | builds the control flow graph for a function
--
astToCFG :: Statement SrcSpan -> Maybe CFG
astToCFG f@(Fun n a _ b _) = runSimpleUniqueMonad (evalStateT createCFG M.empty)
    where createCFG = do
            e <- labelFor f
            m <- get
            graph <- toBlock e b
            return $ Just CFG { name = toName n
                              , args = a
                              , cfgBody = graph
                              , cfgEntryLabel = e
                              , blockLabelMap = m
                              , labelBlockMap = M.fromList $ map swap $ M.toList m
                              }
astToCFG _ = Nothing

toName :: Ident annot -> String
toName (Ident n _) = n


-- | Arguments to IR
--
-- TODO: There are more parameter types to match against.
--
-- toVar :: [Parameter SrcSpan] -> [I.Var]
-- toVar (Param (Ident name _) _ _ an :xs) = I.Variable name (toSrcLocation an) : toVar xs
-- toVar  _                               = []


-- | TODO this does not represent a block in Python, since it only
-- operates on one statement
toBlock :: Label -> Suite SrcSpan -> CFGBuilder (Graph Insn C C)
toBlock eLabel xs = do
    let (bodyBlocks, [lastStmt]) = splitAt (length xs - 1) xs
    let bb = filter isNormalStatement bodyBlocks
    e <- toFirst eLabel
    m <- mapM toMiddle bb
    l <- toLast lastStmt
    return $ mkFirst e <*> mkMiddles m <*> mkLast l


-- | make an entry point IR.Insn
-- TODO I think this is not necessary!
--
toFirst :: Label -> CFGBuilder (Insn C O)
toFirst x = return $ Label x

toMiddle :: Statement SrcSpan -> CFGBuilder (Insn O O)
toMiddle x = return $ Normal x

isNormalStatement :: Statement SrcSpan -> Bool
isNormalStatement Assign{} = True
isNormalStatement AugmentedAssign{} = True
isNormalStatement Delete{} = True
isNormalStatement Print{} = True
isNormalStatement Exec{} = True
isNormalStatement _ = False

toLast :: Statement SrcSpan -> CFGBuilder (Insn O C)
toLast stm = return $ Exit stm

exprToStrings :: Expr annot -> String
exprToStrings (Var (Ident str _) _ ) = str
exprToStrings _                      = ""

-- Helpers
-- These helpers are used for testing purposes and deconstructing the
-- graph in order to visualise it

-- | returns the internal hoopl graph
--
getInternalGraph :: CFG -> Graph Insn C C
getInternalGraph (CFG _ _ _ graph _ _) = graph

getInternalBlockMap :: CFG -> LabelBlockMap
getInternalBlockMap (CFG _ _ _ _ _ lblMap) = lblMap
