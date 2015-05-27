module Fixtures where
import Language.Python.Common.SrcLocation
import Language.Python.Common.AST

-- | fixture providing a Python function with a single assignment and
-- return
--
fixturePythonEmptyFunc :: Statement SrcSpan
fixturePythonEmptyFunc = Fun fun_name fun_args fun_result_annotation fun_body SpanEmpty
    where fun_name = Ident "empty_fun" SpanEmpty
          fun_args = []
          fun_result_annotation = Nothing
          fun_body = [Return Nothing SpanEmpty]

fixturePythonAssignFunc :: Statement SrcSpan
fixturePythonAssignFunc = Fun fun_name fun_args fun_result_annotation fun_body SpanEmpty
    where fun_name = Ident "assign" SpanEmpty
          fun_args = []
          fun_result_annotation = Nothing
          fun_body = [ Assign
                        [ Var (Ident "a" SpanEmpty) SpanEmpty ]
                        (Int 1 "1" SpanEmpty)
                        SpanEmpty
                     , Return
                        (Just (Var (Ident "a" SpanEmpty) SpanEmpty))
                        SpanEmpty
                     ]

