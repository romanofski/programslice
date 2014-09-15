import unittest
from programslice import get_formatter_klass
from programslice.formatter import LineFormatter
from programslice.formatter import VimOutPutFormatter
from programslice.formatter import TextOutputFormatter


class TestCommandLine(unittest.TestCase):

    def test_get_formatter_klass(self):
        self.assertEqual(LineFormatter,
                         get_formatter_klass('linenumbers'))
        self.assertEqual(VimOutPutFormatter,
                         get_formatter_klass('vim'))
        self.assertEqual(TextOutputFormatter,
                         get_formatter_klass('text'))
        self.assertEqual(VimOutPutFormatter,
                         get_formatter_klass('foo'))
