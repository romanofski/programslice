from graphviz import Digraph
from programslice.graph import find_entry_block
from programslice.graph import find_exit_blocks


def draw_cfg(graphs):
    g = Digraph('G', filename='/tmp/foo.gv')
    nodeattrs = dict(shape='rectangle', fontname='Courier', fillcolor='grey', style='filled')

    for graph in graphs:
        entryblock = find_entry_block(graph)
        exitblocks = find_exit_blocks(graph)
        if entryblock is None or not exitblocks:
            raise ValueError("Can't draw graph. Couldn't find entry/exit"
                             " blocks. This is a software error.")

        c = Digraph(graph.name)

        # TODO this can be done better without accessing the internals of the
        # graph
        for key, nodes in graph.graph.items():
            for node in nodes:
                c.node(key.uid, key.name, **nodeattrs)
                c.node(node.uid, node.name, **nodeattrs)
                c.edge(key.uid, node.uid)

        c.node(entryblock.uid, shape='box', fillcolor='white')
        [c.node(x.uid, shape='box', fillcolor='black', fontcolor='white')
         for x in exitblocks]

        c.body.append('label = "{}"'.format(graph.name))
        c.body.append('color=blue')
        g.subgraph(c)

    g.view()
