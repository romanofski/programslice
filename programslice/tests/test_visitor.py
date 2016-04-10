import ast

import programslice.graph
from programslice.visitor import IndentVisitor
from programslice.tests.conftest import visit_source_code


ifelse = '''
def test(x):
    a = 1
    b = 2
    if x == a:
        a = 2
    else:
        a = 3
    return a'''

# TODO
# Does this have an entry and an exit block? Is it only one entry or exit
# block?
simple = '''
def simple():
    a = 1
    return a'''

simple_condition = '''
def test():
    a = 2
    if True:
        a = 3
    b = 3
    return a'''


def assert_entry_exit_blocks(blocks):
    types = [x.type for x in blocks]
    assert programslice.graph.ENTRY in types
    assert programslice.graph.EXIT in types

def test_visitor_ifelse_blocks():
    visitor = visit_source_code(ifelse)
    assert 4 == len(visitor.blocks)
    assert_entry_exit_blocks(visitor.blocks)


def test_visitor_simple_blocks():
    visitor = visit_source_code(simple)
    assert 1 == len(visitor.blocks)


def test_visitor_if_blocks():
    visitor = visit_source_code(simple_condition)
    assert 3 == len(visitor.blocks), simple_condition
    assert_entry_exit_blocks(visitor.blocks)


def test_visitor_multiple_funcs_blocks():
    visitor = visit_source_code('\n'.join([ifelse, simple]))
    assert 6 == len(visitor.blocks)
    assert_entry_exit_blocks(visitor.blocks)
