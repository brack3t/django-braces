#! /usr/bin/env python

# $Id: test_replace.py 4667 2006-07-12 21:40:56Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "replace" directive.
Test in french (not default/fallback language).
"""

from __init__ import DocutilsTestSupport


def suite():
    s = DocutilsTestSupport.ParserTestSuite(suite_settings={'language_code':'fr'})
    s.generateTests(totest)
    return s

totest = {}

totest['replace'] = [
["""\
Test directive containing french role exposant (superscript).

.. |Na+| remplace:: Na\ :exp:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing french role exposant (superscript).
    <substitution_definition names="Na+">
        Na
        <superscript>
            +
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium.
"""],
["""\
Test directive containing english role superscript.

.. |Na+| remplace:: Na\ :sup:`+`

Le |Na+| est l'ion sodium.
""",
"""\
<document source="test data">
    <paragraph>
        Test directive containing english role superscript.
    <system_message level="1" line="3" source="test data" type="INFO">
        <paragraph>
            No role entry for "sup" in module "docutils.parsers.rst.languages.fr".
            Using English fallback for role "sup".
    <substitution_definition names="Na+">
        Na
        <superscript>
            +
    <paragraph>
        Le \n\
        <substitution_reference refname="Na+">
            Na+
         est l\'ion sodium."""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
