#! /usr/bin/env python

# $Id: test_sidebars.py 7062 2011-06-30 22:14:29Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the "sidebar" directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['sidebars'] = [
["""\
.. sidebar:: Outer

   .. sidebar:: Nested

      Body.
""",
"""\
<document source="test data">
    <sidebar>
        <title>
            Outer
        <system_message level="3" line="3" source="test data" type="ERROR">
            <paragraph>
                The "sidebar" directive may not be used within a sidebar element.
            <literal_block xml:space="preserve">
                .. sidebar:: Nested
                \n\
                   Body.
"""],
["""\
.. sidebar:: Margin Notes
   :subtitle: with options
   :class: margin
   :name: note:Options

   Body.
""",
"""\
<document source="test data">
    <sidebar classes="margin" ids="note-options" names="note:options">
        <title>
            Margin Notes
        <subtitle>
            with options
        <paragraph>
            Body.
"""],
["""\
.. sidebar:: Outer

   .. topic:: Topic

      .. sidebar:: Inner

         text
""",
"""\
<document source="test data">
    <sidebar>
        <title>
            Outer
        <topic>
            <title>
                Topic
            <system_message level="3" line="5" source="test data" type="ERROR">
                <paragraph>
                    The "sidebar" directive may not be used within topics or body elements.
                <literal_block xml:space="preserve">
                    .. sidebar:: Inner
                    \n\
                       text
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
