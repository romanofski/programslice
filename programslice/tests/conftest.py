import ast
import pytest
import programslice.visitor


@pytest.fixture(scope='session')
def assignment_graph():
    """ A simple function with basic, numeric assignments."""
    node = ast.parse("""
def foo():
    n=1
    m=1
    o=n+m
    n=o""")
    return visited(node)


@pytest.fixture(scope='session')
def call_graph():
    """ A simple function with basic, numeric assignments."""
    node = ast.parse("""
def foo():
    n=1
    m=bar(n)
    return m

def bar(i):
    j = i + 1
    return j""")
    return visited(node)


def visited(parsed_astmodule):
    visitor = programslice.visitor.LineDependencyVisitor()
    visitor.visit(parsed_astmodule)
    return visitor.graph
