#! /usr/bin/env python

# $Id: test_io.py 7037 2011-05-19 08:56:27Z milde $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for io.py.
"""

import unittest, sys
import DocutilsTestSupport              # must be imported before docutils
from docutils import io
from docutils._compat import b, bytes


class InputTests(unittest.TestCase):

    def test_bom(self):
        input = io.StringInput(source=b('\xef\xbb\xbf foo \xef\xbb\xbf bar'),
                               encoding='utf8')
        # Assert BOMs are gone.
        self.assertEquals(input.read(), u' foo  bar')
        # With unicode input:
        input = io.StringInput(source=u'\ufeff foo \ufeff bar')
        # Assert BOMs are still there.
        self.assertEquals(input.read(), u'\ufeff foo \ufeff bar')

    def test_coding_slug(self):
        input = io.StringInput(source=b("""\
.. -*- coding: ascii -*-
data
blah
"""))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'ascii')
        input = io.StringInput(source=b("""\
#! python
# extraneous comment; prevents coding slug from being read
# -*- coding: ascii -*-
print "hello world"
"""))
        data = input.read()
        self.assertNotEquals(input.successful_encoding, 'ascii')

    def test_bom_detection(self):
        source = u'\ufeffdata\nblah\n'
        input = io.StringInput(source=source.encode('utf-16-be'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-16-be')
        input = io.StringInput(source=source.encode('utf-16-le'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-16-le')
        input = io.StringInput(source=source.encode('utf-8'))
        data = input.read()
        self.assertEquals(input.successful_encoding, 'utf-8')


if __name__ == '__main__':
    unittest.main()
