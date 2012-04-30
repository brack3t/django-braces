#! /usr/bin/env python

# $Id: test_date.py 4667 2006-07-12 21:40:56Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the misc.py "date" directive.
"""

from __init__ import DocutilsTestSupport
import time


def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['date'] = [
["""\
.. |date| date::

Today's date is |date|.
""",
"""\
<document source="test data">
    <substitution_definition names="date">
        %s
    <paragraph>
        Today's date is \n\
        <substitution_reference refname="date">
            date
        .
""" % time.strftime('%Y-%m-%d')],
["""\
.. |date| date:: %a, %d %b %Y
""",
"""\
<document source="test data">
    <substitution_definition names="date">
        %s
""" % time.strftime('%a, %d %b %Y')],
["""\
.. date::
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Invalid context: the "date" directive can only be used within a substitution definition.
        <literal_block xml:space="preserve">
            .. date::
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
