#! /usr/bin/env python

# $Id: test_parser.py 5889 2009-04-01 20:00:21Z gbrandl $
# Author: Stefan Rank <strank(AT)strank(DOT)info>
# Copyright: This module has been placed in the public domain.

"""
Tests for basic functionality of parser classes.
"""

import sys
import unittest
import DocutilsTestSupport              # must be imported before docutils
import docutils
from docutils import parsers, utils, frontend
from docutils._compat import b


class RstParserTests(unittest.TestCase):

    def test_inputrestrictions(self):
        parser_class = parsers.get_parser_class('rst')
        parser = parser_class()
        document = utils.new_document('test data', frontend.OptionParser(
                    components=(parser, )).get_default_values())

        if sys.version_info < (3,):
            # supplying string input is supported, but only if ascii-decodable
            self.assertRaises(UnicodeError, # UnicodeDecodeError since py2.3
                              parser.parse, b('hol%s' % chr(224)), document)
        else:
            # input must be unicode at all times
            self.assertRaises(TypeError, parser.parse, b('hol'), document)


if __name__ == '__main__':
    unittest.main()
