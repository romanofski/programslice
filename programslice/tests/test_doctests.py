import unittest2 as unittest
import doctest
import os.path


OPTIONFLAGS = doctest.NORMALIZE_WHITESPACE | doctest.ELLIPSIS
DOCFILES = [
    'slice_forward.txt'
]


def test_suite():
    suite = unittest.TestSuite()
    suite.addTests([
        doctest.DocFileSuite(
            os.path.join(os.path.dirname(__file__), docfile),
            module_relative=False,
            optionflags=OPTIONFLAGS,
            ) for docfile in DOCFILES]
    )
    return suite
