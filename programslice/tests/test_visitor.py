import ast

from programslice.visitor import IndentVisitor


condition = '''
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


def test_visitor_finds_correct_basic_blocks():
    code = [simple_condition,
            condition,
            simple, '\n'.join([condition, simple]),
            ]
    expected = [3, 4, 1, 6]

    for source, exp_blocks in zip(code, expected):
        node = ast.parse(source.strip(), 'testdata')
        visitor = IndentVisitor()
        visitor.visit(node)
        assert len(visitor.blocks) == exp_blocks, source
