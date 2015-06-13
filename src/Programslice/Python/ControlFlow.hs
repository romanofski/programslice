{-# LANGUAGE RankNTypes, GADTs #-}
module Programslice.Python.ControlFlow where

import Compiler.Hoopl
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Control.Monad
import qualified Data.Map as M


data Insn e x where
    Label   :: Label -> Insn C O
    Normal  :: Statement SrcSpan -> Label -> Insn O O
    Exit    :: Maybe (Expr SrcSpan) -> Insn O C


-- | A representation of a control flow graph
--
-- Note: The current intention of constructing control flow graphs is
-- on a function level, until I can utilise Hoopl better.
--
data CFG = CFG { name :: String                     -- ^ function name
                , args :: [Parameter SrcSpan]    -- ^ function parameters
                , entry :: Label                    -- ^ function entry label
                , body :: Graph Insn C C            -- ^ CFG of the function
                }


instance NonLocal (Insn) where
    entryLabel (Label l)    = l
    successors (Exit _ )   = []


instance Show (CFG) where
    show (CFG {name = n, args = _, entry = lbl, body = g }) =
        show $ n ++ show lbl ++ ": " ++ graph ++ "||"
        where graph = showGraph show g


instance Show (Insn e x) where
    show (Label lbl)  = show lbl ++ ":"
    show (Normal _ lbl) = show lbl ++ "N"
    show (Exit (Just xs)) = show xs
    show (Exit Nothing) = ""


-- | Map to avoid creating new labels for already known identifiers
--
type IdLabelMap = M.Map String Label
data LabelMapM a = LabelMapM (IdLabelMap -> SimpleFuelMonad (IdLabelMap, a))

-- | TODO urks... what are we doing here?
--
instance Monad LabelMapM where
    return x = LabelMapM (\m -> return (m, x))

    LabelMapM f1 >>= k = LabelMapM (\m -> do (m', x) <- f1 m
                                             let (LabelMapM f2) = k x
                                             f2 m')

-- | main function to convert Python AST to IR
--
astToCFG :: Statement SrcSpan -> SimpleFuelMonad (IdLabelMap, CFG)
astToCFG (Fun {  fun_name = n
              , fun_args = a
              , fun_result_annotation = _
              , fun_body = b
              , stmt_annot = _ })
    = run $ do
        e <- getEntry n
        graph <- toBody b
        return CFG { name = toName n, args = a, body = graph, entry = e }

run :: LabelMapM a -> SimpleFuelMonad (IdLabelMap, a)
run (LabelMapM f) = f M.empty

-- | returns a label for the given "function" name
--
getEntry :: Ident annot -> LabelMapM Label
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


toBody :: Suite SrcSpan -> LabelMapM (Graph Insn C C)
toBody xs =
    do g <- foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map toBlock xs)
       getBody g

-- | TODO this does not represent a block in Python, since it only
-- operates on one statement
toBlock :: Statement SrcSpan -> LabelMapM (Graph Insn C C)
toBlock x = toFirst x >>= \f ->
                toMiddle x >>= \m ->
                    toLast x >>= \l ->
                        return $ mkFirst f <*> mkMiddle m <*> mkLast l


-- | make an entry point IR.Insn
-- TODO I think this is not necessary!
--
toFirst :: Statement SrcSpan -> LabelMapM (Insn C O)
toFirst x = liftM Label $ labelFor (show x)

toMiddle :: Statement SrcSpan -> LabelMapM (Insn O O)
toMiddle x = do
    lbl <- labelFor $ show x
    return $ Normal x lbl

toLast :: Statement SrcSpan -> LabelMapM (Insn O C)
toLast (Return x _) = return $ Exit x
toLast _ = return $ Exit Nothing

exprToStrings :: Expr annot -> String
exprToStrings (Var (Ident str _) _ ) = str
exprToStrings _                      = ""

getBody :: forall n. Graph n C C -> LabelMapM (Graph n C C)
getBody graph = LabelMapM f
    where f m = return (m, graph)

-- | Use the LabelMapM Monad to create a new mapping in the label map.
-- If we have already created a label return the given label, otherwise
-- create a new one and return it.
--
labelFor :: String -> LabelMapM Label
labelFor name = LabelMapM go
    where go m = case M.lookup name m of
            Just l' -> return (m, l')
            Nothing -> do l' <- freshLabel
                          let m' = M.insert name l' m
                          return (m', l')

