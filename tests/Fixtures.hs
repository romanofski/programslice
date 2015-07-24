{-# LANGUAGE NamedFieldPuns #-}
module Fixtures where
import Programslice.Parse
import Programslice.Python.ControlFlow (CFG)

assignFunction :: String
assignFunction = "def issue_1():\n\
                  \   a = 1\n\
                  \   b = 2\n\
                  \   a = a - b\n\
                  \   return a\n"

conditionFunction :: String
conditionFunction = "def get_bool():\n\
                    \    a = 1\n\
                    \    if a == 1:\n\
                    \        b = 2\n\
                    \    else:\n\
                    \        b = 3\n\
                    \    return b"

simpleFunctionFixture :: Maybe CFG
simpleFunctionFixture = parse assignFunction

multipleFunctions :: Maybe CFG
multipleFunctions = parse contents
  where contents = conditionFunction ++ "\n\n" ++ assignFunction
