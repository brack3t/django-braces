#! /usr/bin/env python

# $Id: test_block_quotes.py 4564 2006-05-21 20:44:42Z wiemann $
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

totest['block_quotes'] = [
["""\
Line 1.
Line 2.

   Indented.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented.
"""],
["""\
Line 1.
Line 2.

   Indented 1.

      Indented 2.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented 1.
        <block_quote>
            <paragraph>
                Indented 2.
"""],
["""\
Line 1.
Line 2.
    Unexpectedly indented.
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <system_message level="3" line="3" source="test data" type="ERROR">
        <paragraph>
            Unexpected indentation.
    <block_quote>
        <paragraph>
            Unexpectedly indented.
"""],
["""\
Line 1.
Line 2.

   Indented.
no blank line
""",
"""\
<document source="test data">
    <paragraph>
        Line 1.
        Line 2.
    <block_quote>
        <paragraph>
            Indented.
    <system_message level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Block quote ends without a blank line; unexpected unindent.
    <paragraph>
        no blank line
"""],
["""\
Here is a paragraph.

        Indent 8 spaces.

    Indent 4 spaces.

Is this correct? Should it generate a warning?
Yes, it is correct, no warning necessary.
""",
"""\
<document source="test data">
    <paragraph>
        Here is a paragraph.
    <block_quote>
        <block_quote>
            <paragraph>
                Indent 8 spaces.
        <paragraph>
            Indent 4 spaces.
    <paragraph>
        Is this correct? Should it generate a warning?
        Yes, it is correct, no warning necessary.
"""],
["""\
Paragraph.

   Block quote.

   -- Attribution

Paragraph.

   Block quote.

   --Attribution
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution
"""],
[u"""\
Alternative: true em-dash.

   Block quote.

   \u2014 Attribution

Alternative: three hyphens.

   Block quote.

   --- Attribution
""",
"""\
<document source="test data">
    <paragraph>
        Alternative: true em-dash.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution
    <paragraph>
        Alternative: three hyphens.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution
"""],
["""\
Paragraph.

   Block quote.

   -- Attribution line one
   and line two

Paragraph.

   Block quote.

   -- Attribution line one
      and line two

Paragraph.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution line one
            and line two
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <attribution>
            Attribution line one
            and line two
    <paragraph>
        Paragraph.
"""],
["""\
Paragraph.

   Block quote 1.

   -- Attribution 1

   Block quote 2.

   --Attribution 2
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote 1.
        <attribution>
            Attribution 1
    <block_quote>
        <paragraph>
            Block quote 2.
        <attribution>
            Attribution 2
"""],
["""\
Paragraph.

   Block quote 1.

   -- Attribution 1

   Block quote 2.
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote 1.
        <attribution>
            Attribution 1
    <block_quote>
        <paragraph>
            Block quote 2.
"""],
["""\
Unindented paragraph.

    Block quote 1.

    -- Attribution 1

    Block quote 2.

..

    Block quote 3.
""",
"""\
<document source="test data">
    <paragraph>
        Unindented paragraph.
    <block_quote>
        <paragraph>
            Block quote 1.
        <attribution>
            Attribution 1
    <block_quote>
        <paragraph>
            Block quote 2.
    <comment xml:space="preserve">
    <block_quote>
        <paragraph>
            Block quote 3.
"""],
["""\
Paragraph.

   -- Not an attribution

Paragraph.

   Block quote.

   \-- Not an attribution

Paragraph.

   Block quote.

   -- Not an attribution line one
      and line two
          and line three
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            -- Not an attribution
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <paragraph>
            -- Not an attribution
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            Block quote.
        <definition_list>
            <definition_list_item>
                <term>
                    -- Not an attribution line one
                <definition>
                    <definition_list>
                        <definition_list_item>
                            <term>
                                and line two
                            <definition>
                                <paragraph>
                                    and line three
"""],
["""\
Paragraph.

   -- Not a valid attribution

   Block quote 1.

   --Attribution 1

   --Invalid attribution

   Block quote 2.

   --Attribution 2
""",
"""\
<document source="test data">
    <paragraph>
        Paragraph.
    <block_quote>
        <paragraph>
            -- Not a valid attribution
        <paragraph>
            Block quote 1.
        <attribution>
            Attribution 1
    <block_quote>
        <paragraph>
            --Invalid attribution
        <paragraph>
            Block quote 2.
        <attribution>
            Attribution 2
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
