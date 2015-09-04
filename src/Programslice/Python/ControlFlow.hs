{-# LANGUAGE RankNTypes, GADTs #-}
module Programslice.Python.ControlFlow where

import Compiler.Hoopl
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST()
import Control.Monad.State
import Data.Tuple (swap)
import Data.Maybe (catMaybes)
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
data CFG = CFG {  cfgEntryLabel :: Label            -- ^ function entry label
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
    show (CFG {cfgEntryLabel = lbl, cfgBody = g }) =
        show $ show lbl ++ "\n:" ++ graph
        where graph = showGraph prettyText g

-- | A control flow instruction
--
data Insn e x where
    Label   :: Label -> Insn C O
    Condition :: Label -> Maybe Label -> Insn O C
    Normal  :: Statement SrcSpan -> Insn O O
    Exit    :: Statement SrcSpan -> Insn O C

-- | Nonlocal control flow
--
instance NonLocal (Insn) where
    entryLabel (Label l)  = l
    successors (Condition c Nothing) = [c]
    successors (Condition c (Just e)) = [c, e]
    successors (Exit  _)  = []

instance Show (Insn e x) where
    show (Label lbl)  = "[CO]" ++ show lbl ++ ":"
    show (Condition c (Just e)) = "if .. : " ++ show c ++ "else: " ++ show e
    show (Condition c Nothing) = "if ... : " ++ show c
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
moduleToCFG :: Module SrcSpan -> Maybe CFG
moduleToCFG (Module xs) = runSimpleUniqueMonad (evalStateT (createCFG xs) M.empty)

createCFG :: [Statement SrcSpan] -> CFGBuilder (Maybe CFG)
createCFG xs = do
        maybeGraphs <- mapM astToCFG xs
        m <- get
        let g' = catMaybes maybeGraphs
        -- meh this condition.
        -- TODO: can this be implemented better?
        if null g' then return Nothing
        else do
            let (e, _) = head g'
            let graphs = foldl (|*><*|) emptyClosedGraph (fmap snd g')
            return $ Just CFG { cfgBody = graphs
                              , cfgEntryLabel = e
                              , blockLabelMap = m
                              , labelBlockMap = M.fromList $ map swap $ M.toList m
                              }


astToCFG :: Statement SrcSpan -> CFGBuilder (Maybe (Label, Graph Insn C C))
astToCFG (Fun n args _ b a) = do
            e <- labelFor function
            graph <- toBlock e b
            return $ Just (e, graph)
            where function = Fun { fun_name = n
                                 , fun_args = args
                                 , fun_result_annotation = Nothing
                                 , fun_body = []
                                 , stmt_annot = a
                                 }
astToCFG _ = return Nothing

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
    let bb = fmap toBodyBlocks bodyBlocks
    e <- toFirst eLabel
    m <- mapM toMiddle bb
    l <- toLast lastStmt
    return $ mkFirst e <*> mkMiddles m <*> mkLast l


-- | make an entry point IR.Insn
-- TODO I think this is not necessary!
--
toFirst :: Label -> CFGBuilder (Insn C O)
toFirst x = return $ Label x

-- TODO how to create an edge to the first statement after the condition?
toMiddle :: Statement SrcSpan -> CFGBuilder (Insn O O)
toMiddle (Conditional guards c_else annot) = return $ fmap createConditions
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
