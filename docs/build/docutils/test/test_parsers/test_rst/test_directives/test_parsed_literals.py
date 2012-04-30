#! /usr/bin/env python

# $Id: test_parsed_literals.py 7072 2011-07-06 15:52:30Z milde $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Tests for the body.py 'parsed-literal' directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['parsed_literals'] = [
["""\
.. parsed-literal::

   This is a parsed literal block.
   It may contain *inline markup
   spanning lines.*
""",
"""\
<document source="test data">
    <literal_block xml:space="preserve">
        This is a parsed literal block.
        It may contain \n\
        <emphasis>
            inline markup
            spanning lines.
"""],
["""\
.. parsed-literal::
  :class: myliteral
  :name: example: parsed

   This is a parsed literal block with options.
""",
"""\
<document source="test data">
    <literal_block classes="myliteral" ids="example-parsed" names="example:\ parsed" xml:space="preserve">
         This is a parsed literal block with options.
"""],
["""\
.. parsed-literal:: content may start on same line
""",
"""\
<document source="test data">
    <literal_block xml:space="preserve">
        content may start on same line
"""],
["""\
.. parsed-literal::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "parsed-literal" directive; none found.
        <literal_block xml:space="preserve">
            .. parsed-literal::
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
