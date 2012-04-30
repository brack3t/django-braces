#! /usr/bin/env python

# $Id: test_citations.py 5510 2008-02-15 09:23:07Z grubert $
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

totest['citations'] = [
["""\
.. [citation] This is a citation.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation.
"""],
["""\
.. [citation1234] This is a citation with year.
""",
"""\
<document source="test data">
    <citation ids="citation1234" names="citation1234">
        <label>
            citation1234
        <paragraph>
            This is a citation with year.
"""],
["""\
.. [citation] This is a citation
   on multiple lines.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation
            on multiple lines.
"""],
["""\
.. [citation1] This is a citation
     on multiple lines with more space.

.. [citation2] This is a citation
  on multiple lines with less space.
""",
"""\
<document source="test data">
    <citation ids="citation1" names="citation1">
        <label>
            citation1
        <paragraph>
            This is a citation
            on multiple lines with more space.
    <citation ids="citation2" names="citation2">
        <label>
            citation2
        <paragraph>
            This is a citation
            on multiple lines with less space.
"""],
["""\
.. [citation]
   This is a citation on multiple lines
   whose block starts on line 2.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
        <paragraph>
            This is a citation on multiple lines
            whose block starts on line 2.
"""],
["""\
.. [citation]

That was an empty citation.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
    <paragraph>
        That was an empty citation.
"""],
["""\
.. [citation]
No blank line.
""",
"""\
<document source="test data">
    <citation ids="citation" names="citation">
        <label>
            citation
    <system_message level="2" line="2" source="test data" type="WARNING">
        <paragraph>
            Explicit markup ends without a blank line; unexpected unindent.
    <paragraph>
        No blank line.
"""],
["""\
.. [citation label with spaces] this isn't a citation

.. [*citationlabelwithmarkup*] this isn't a citation
""",
"""\
<document source="test data">
    <comment xml:space="preserve">
        [citation label with spaces] this isn't a citation
    <comment xml:space="preserve">
        [*citationlabelwithmarkup*] this isn't a citation
"""],
["""
isolated internals : ``.-_``.

.. [citation.withdot] one dot

.. [citation-withdot] one hyphen

.. [citation_withunderscore] one underscore

.. [citation:with:colons] two colons

.. [citation+withplus] one plus
""",
"""<document source="test data">
    <paragraph>
        isolated internals : \n\
        <literal>
            .-_
        .
    <citation ids="citation-withdot" names="citation.withdot">
        <label>
            citation.withdot
        <paragraph>
            one dot
    <citation ids="id1" names="citation-withdot">
        <label>
            citation-withdot
        <paragraph>
            one hyphen
    <citation ids="citation-withunderscore" names="citation_withunderscore">
        <label>
            citation_withunderscore
        <paragraph>
            one underscore
    <citation ids="citation-with-colons" names="citation:with:colons">
        <label>
            citation:with:colons
        <paragraph>
            two colons
    <citation ids="citation-withplus" names="citation+withplus">
        <label>
            citation+withplus
        <paragraph>
            one plus
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
