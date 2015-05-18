{-# LANGUAGE RankNTypes #-}
module PythonAst2IR where

import Compiler.Hoopl
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Control.Monad
import qualified IR as I
import qualified Data.Map as M


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
astToIR :: Statement SrcSpan -> SimpleFuelMonad I.Proc
astToIR (Fun {  fun_name = n
              , fun_args = a
              , fun_result_annotation = _
              , fun_body = b
              , stmt_annot = _ })
    = run $ do
        entry <- getEntry n
        body <- toBody b
        return I.Proc { I.name = toName n, I.args = toVar a, I.body = body, I.entry = entry }

run :: LabelMapM a -> SimpleFuelMonad a
run (LabelMapM f) = liftM snd $ f M.empty

-- | returns a label for the given "function" name
--
getEntry :: Ident annot -> LabelMapM Label
getEntry x = labelFor $ toName x

toName :: Ident annot -> String
toName (Ident name _) = name


-- | Arguments to IR
--
-- TODO: There are more parameter types to match against.
--
toVar :: [Parameter SrcSpan] -> [I.Var]
toVar (Param (Ident name _) _ _ an :xs) = I.Variable name (toSrcLocation an) : toVar xs
toVar  _                               = []


toBody :: Suite SrcSpan -> LabelMapM (Graph (I.Insn I.SrcLocation) C C)
toBody x =
    do g <- foldl (liftM2 (|*><*|)) (return emptyClosedGraph) (map toBlock x)
       getBody g

-- | TODO this does not represent a block in Python, since it only
-- operates on one statement
toBlock :: Statement SrcSpan -> LabelMapM (Graph (I.Insn I.SrcLocation) C C)
toBlock x = toFirst x >>= \f ->
                toLast x >>= \l ->
                    return $ mkFirst f <*> mkLast l


-- | make an entry point IR.Insn
-- TODO non exaustive patterns!
--
toFirst :: Statement SrcSpan -> LabelMapM ((I.Insn I.SrcLocation) C O)
toFirst x = liftM I.Label $ labelFor (show x)
toFirst (Assign to _ _) = liftM I.Label $ labelFor entry
    where entry = exprToStrings $ head to

toLast :: Statement SrcSpan -> LabelMapM ((I.Insn I.SrcLocation) O C)
toLast (Return (Just x) an) = return $ I.Return $ Just [I.Variable (exprToStrings x) (toSrcLocation an)]
toLast _ = return $ I.Return Nothing

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


-- | Helper function to convert language annotation to our annotation
--
toSrcLocation :: SrcSpan -> I.SrcLocation
toSrcLocation (SpanPoint _ r c) = I.SingleLocation r c
toSrcLocation (SpanCoLinear _ r sc ec) = I.CoLinearLocation r sc ec
toSrcLocation (SpanMultiLine _ sr sc er ec) = I.MultiLineLocation sr sc er ec
