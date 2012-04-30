#! /usr/bin/env python

# $Id: test_doctest_blocks.py 4564 2006-05-21 20:44:42Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for states.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['doctest_blocks'] = [
["""\
Paragraph.

>>> print "Doctest block."
Doctest block.

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <doctest_block xml:space="preserve">
        >>> print "Doctest block."
        Doctest block.
    <paragraph>
        Paragraph.
"""],
["""\
Paragraph.

>>> print "    Indented output."
    Indented output.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <doctest_block xml:space="preserve">
        >>> print "    Indented output."
            Indented output.
"""],
["""\
Paragraph.

    >>> print "    Indented block & output."
        Indented block & output.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <doctest_block xml:space="preserve">
            >>> print "    Indented block & output."
                Indented block & output.
"""],
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
