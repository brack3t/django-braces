#! /usr/bin/env python

# $Id: test_compound.py 7072 2011-07-06 15:52:30Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the 'compound' directive from body.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['compound'] = [
["""\
.. compound::

   Compound paragraphs are single logical paragraphs
   which contain embedded

   * lists
   * tables
   * literal blocks
   * and other body elements

   and are split into multiple physical paragraphs.
""",
"""\
<document source="test data">
    <compound>
        <paragraph>
            Compound paragraphs are single logical paragraphs
            which contain embedded
        <bullet_list bullet="*">
            <list_item>
                <paragraph>
                    lists
            <list_item>
                <paragraph>
                    tables
            <list_item>
                <paragraph>
                    literal blocks
            <list_item>
                <paragraph>
                    and other body elements
        <paragraph>
            and are split into multiple physical paragraphs.
"""],
["""\
.. compound::
   :name: interesting
   :class: log

   This is an extremely interesting compound paragraph containing a
   simple paragraph, a literal block with some useless log messages::

       Connecting... OK
       Transmitting data... OK
       Disconnecting... OK

   and another simple paragraph which is actually just a continuation
   of the first simple paragraph, with the literal block in between.
""",
"""\
<document source="test data">
    <compound classes="log" ids="interesting" names="interesting">
        <paragraph>
            This is an extremely interesting compound paragraph containing a
            simple paragraph, a literal block with some useless log messages:
        <literal_block xml:space="preserve">
            Connecting... OK
            Transmitting data... OK
            Disconnecting... OK
        <paragraph>
            and another simple paragraph which is actually just a continuation
            of the first simple paragraph, with the literal block in between.
"""],
["""\
.. compound:: content may start on same line

   second paragraph
""",
"""\
<document source="test data">
    <compound>
        <paragraph>
            content may start on same line
        <paragraph>
            second paragraph
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
