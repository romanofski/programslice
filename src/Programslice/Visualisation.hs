{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Programslice.Visualisation where

import           Compiler.Hoopl
import qualified Data.GraphViz                   as GV
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Language.Python.Common.Pretty   (prettyText)
import           Prelude                         hiding (lookup)
import           Programslice.Parse              (parse)
import           Programslice.Python.ControlFlow (CFG (..))


cfgGraphvizParams :: GV.GraphvizParams n String String () String
cfgGraphvizParams =
  GV.nonClusteredParams { GV.fmtNode = \(_, l) -> [GV.toLabel l]
                        , GV.fmtEdge = \(_, _, l) -> [GV.toLabel l]}

cfgGraphvizRepr :: CFG -> GV.DotGraph Int
cfgGraphvizRepr cfg = GV.graphElemsToDot cfgGraphvizParams (concat ns) es
  where
    ns = buildGraphNodes 1 $ buildGraphNodeLabels cfg
    es = buildGraphEdges ns


-- | How to transform a hoopl graph to a dot graph
--
-- 1. extract the labels and create a list of lists of node labels.
-- We lookup source code fragments in our label to block
-- mapping for the entry label and each successors.
--
-- TODO: Currently the structure is very fuzzy. the successors should form an
-- edge. What if there are more than one successors or more complex types
-- representing edges (e.g. conditions, branches, etc)
--
-- 2. Assumption is control flow goes linear through the list of lists (TODO: --
-- That is most likely a mistake! What about jumps, branches? How to represent
-- that?)

-- | transforms list of node labels into nodes
--
-- >>> let xs = [["a","b"], ["c"]]
-- >>> buildGraphNodes 0 xs
-- [[(0,"a"),(1,"b")],[(2,"c")]]
--
buildGraphNodes :: Int -> [[String]] -> [[(Int, String)]]
buildGraphNodes _ [] = []
buildGraphNodes i (ys:xs) = numbered : buildGraphNodes ( i' + 1 ) xs
  where numbered = zip [i..] ys
        i' = fst $ last numbered

-- | Build edges from all nodes
-- We take the last tuple of the first list and the first tuple of the next list
-- and create an edge from there.
--
buildGraphEdges :: [[(Int, String)]] -> [(Int, Int, String)]
buildGraphEdges [] = []
buildGraphEdges [_] = []
buildGraphEdges (ys:xs:rest) = (x, y, "") : buildGraphEdges rest
  where x = fst $ last ys
        y = fst $ head xs


-- | builds the graph node labels as a list of lists
--
buildGraphNodeLabels :: CFG -> [[String]]
buildGraphNodeLabels cfg = filter (not . null) [ eNodes, succNodes ]
  where
    eLabel = cfgEntryLabel cfg
    eNodes = maybeToList $ labelToBasicBlock cfg eLabel
    succNodes = mapMaybe ( labelToBasicBlock cfg ) (successorBlocks cfg eLabel)

-- | return all successor labels for given label
--
successorBlocks :: CFG -> Label -> [Label]
successorBlocks cfg lbl = case cfgBody cfg of
  (GMany _ blockmap _) -> fromMaybe [] $ do
    basicBlock <- mapLookup lbl blockmap
    return $ successors basicBlock

-- | look up source fragment in the label to block mapping
--
labelToBasicBlock :: CFG -> Label -> Maybe String
labelToBasicBlock cfg lbl = fmap prettyText block
  where block = M.lookup lbl (labelBlockMap cfg)

testGraph :: CFG
testGraph = cfg
    where contents = "def issue_1():\n    a = 1\n    b = 2\n    a = a - b\n    return a\n"
          cfg = head $ parse contents
