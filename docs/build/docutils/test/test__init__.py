#! /usr/bin/env python
# .. coding: utf8
# $Id: test__init__.py 7007 2011-04-12 09:10:57Z milde $
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test module for the docutils' __init__.py.
"""

import unittest
import sys
import DocutilsTestSupport              # must be imported before docutils
import docutils

class ApplicationErrorTests(unittest.TestCase):

    def test_message(self):
        err = docutils.ApplicationError('the message')
        self.assertEquals(unicode(err), u'the message')

    def test_non_ASCII_message(self):
        err = docutils.ApplicationError(u'\u0169')
        self.assertEquals(unicode(err), u'\u0169')


if __name__ == '__main__':
    unittest.main()
