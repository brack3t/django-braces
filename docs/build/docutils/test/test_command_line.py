#! /usr/bin/env python
# .. coding: utf8
# $Id: test_command_line.py 7070 2011-07-05 10:13:17Z milde $
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the command line.
"""

import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils.core

try:
    import locale # module missing in Jython
    locale_encoding = locale.getdefaultlocale()[1]
except ImportError:
    locale_encoding = None

class CommandLineEncodingTests(unittest.TestCase):

    def test_sys_argv_decoding(self):
        if locale_encoding in (None, 'ascii'): # cannot test
            return
        sys.argv.append('--source-url=test.txt') # pure ASCII argument
        if sys.version_info < (3,0):
            sys.argv.append(u'--title=Dornröschen'.encode(locale_encoding))
        else:
            sys.argv.append(u'--title=Dornröschen')
        publisher = docutils.core.Publisher()
        publisher.process_command_line()
        self.assertEqual(publisher.settings.source_url, 'test.txt')
        self.assertEqual(publisher.settings.title, u'Dornröschen')
        sys.argv.pop()

if __name__ == '__main__':
    unittest.main()
