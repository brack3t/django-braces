#! /usr/bin/env python

# $Id: test_rubrics.py 7062 2011-06-30 22:14:29Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the "rubric" directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['rubrics'] = [
["""\
.. rubric:: This is a rubric
""",
"""\
<document source="test data">
    <rubric>
        This is a rubric
"""],
["""\
.. rubric::
.. rubric:: A rubric has no content

   Invalid content
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Error in "rubric" directive:
            1 argument(s) required, 0 supplied.
        <literal_block xml:space="preserve">
            .. rubric::
    <system_message level="3" line="2" source="test data" type="ERROR">
        <paragraph>
            Error in "rubric" directive:
            no content permitted.
        <literal_block xml:space="preserve">
            .. rubric:: A rubric has no content
            \n\
               Invalid content
"""],
["""\
.. rubric:: A rubric followed by a block quote
..

   Block quote
""",
"""\
<document source="test data">
    <rubric>
        A rubric followed by a block quote
    <comment xml:space="preserve">
    <block_quote>
        <paragraph>
            Block quote
"""],
["""\
.. rubric:: A Rubric
   :class: foo bar
   :name: Foo Rubric
""",
"""\
<document source="test data">
    <rubric classes="foo bar" ids="foo-rubric" names="foo\ rubric">
        A Rubric
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
