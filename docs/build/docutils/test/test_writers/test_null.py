#!/usr/bin/env python

# $Id: test_null.py 5174 2007-05-31 00:01:52Z wiemann $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test for Null writer.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.PublishTestSuite('null')
    s.generateTests(totest)
    return s

totest = {}

totest['basic'] = [
["""\
This is a paragraph.
""",
None]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
