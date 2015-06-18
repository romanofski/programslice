{-# LANGUAGE RankNTypes, GADTs #-}
module Programslice.Python.ControlFlow where

import Compiler.Hoopl
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Control.Monad
import Control.Monad.State
import Data.Tuple (swap)
import qualified Data.Map as M


-- | Map to avoid creating new labels for already known identifiers
--
type IdLabelMap = M.Map String Label
type LabelBlockMap = M.Map Label String

-- | A representation of a control flow graph
--
-- Note: The current intention of constructing control flow graphs is
-- on a function level, until I can utilise Hoopl better.
--
data CFG = CFG { name :: String                     -- ^ function name
                , args :: [Parameter SrcSpan]       -- ^ function parameters
                , entry :: Label                    -- ^ function entry label
                , body :: Graph Insn C C            -- ^ CFG of the function
                , blockLabelMap :: IdLabelMap            -- ^ maps source code blocks to labels
                , labelBlockMap :: LabelBlockMap         -- ^ maps labels to code blocks
                }


-- | Monadic environment making sure that we generate a fresh unique
-- label for basic blocks or return a label if we've seen the basic
-- block.
--
type CFGBuilder a = StateT IdLabelMap SimpleUniqueMonad a

instance Show (CFG) where
    show (CFG {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g



-- | A control flow instruction
--
data Insn e x where
    Label   :: Label -> Insn C O
    Normal  :: Statement SrcSpan -> Label -> Insn O O
    Exit    :: Maybe (Expr SrcSpan) -> Insn O C

-- | Nonlocal control flow
--
instance NonLocal (Insn) where
    entryLabel (Label l)   = l
    successors (Exit _ )   = []

instance Show (Insn e x) where
    show (Label lbl)  = "[CO]" ++ show lbl ++ ":"
    show (Normal _ lbl) = "[OO]" ++ show lbl
    show (Exit (Just xs)) = "[OC]" ++ show xs
    show (Exit Nothing) = "[OC]"


-- | Creates a new label for the given string or source code block.
-- If we have already created a label return the given label, otherwise
-- create a new one and return it.
--
labelFor :: String -> CFGBuilder Label
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
astToCFG (Fun n a _ b _) = runSimpleUniqueMonad (evalStateT createCFG M.empty)
    where createCFG = do
            m <- get
            e <- getEntry n
            graph <- toBody b
            return $ Just CFG { name = toName n
                       , args = a
                       , body = graph
                       , entry = e
                       , blockLabelMap = m
                       , labelBlockMap = M.fromList $ map swap $ M.toList m
                       }
astToCFG _ = Nothing

-- | returns a label for the given "function" name
--
getEntry :: Ident annot -> CFGBuilder Label
getEntry x = labelFor $ toName x

toName :: Ident annot -> String
toName (Ident n _) = n


-- | Arguments to IR
--
-- TODO: There are more parameter types to match against.
--
-- toVar :: [Parameter SrcSpan] -> [I.Var]
-- toVar (Param (Ident name _) _ _ an :xs) = I.Variable name (toSrcLocation an) : toVar xs
-- toVar  _                               = []


toBody :: Suite SrcSpan -> CFGBuilder (Graph Insn C C)
toBody xs = foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map toBlock xs)

-- | TODO this does not represent a block in Python, since it only
-- operates on one statement
toBlock :: Statement SrcSpan -> CFGBuilder (Graph Insn C C)
toBlock x = toFirst x >>= \f ->
                toMiddle x >>= \m ->
                    toLast x >>= \l ->
                        return $ mkFirst f <*> mkMiddle m <*> mkLast l


-- | make an entry point IR.Insn
-- TODO I think this is not necessary!
--
toFirst :: Statement SrcSpan -> CFGBuilder (Insn C O)
toFirst x = liftM Label $ labelFor (show x)

toMiddle :: Statement SrcSpan -> CFGBuilder (Insn O O)
toMiddle x = do
    lbl <- labelFor $ show x
    return $ Normal x lbl

toLast :: Statement SrcSpan -> CFGBuilder (Insn O C)
toLast (Return x _) = return $ Exit x
toLast _ = return $ Exit Nothing

exprToStrings :: Expr annot -> String
exprToStrings (Var (Ident str _) _ ) = str
exprToStrings _                      = ""
