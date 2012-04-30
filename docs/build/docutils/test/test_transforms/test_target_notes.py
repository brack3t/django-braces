#! /usr/bin/env python

# $Id: test_target_notes.py 4564 2006-05-21 20:44:42Z wiemann $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for `docutils.transforms.references.TargetNotes` (via
`docutils.transforms.universal.LastReaderPending`).
"""

from __init__ import DocutilsTestSupport
from docutils.transforms.references import PropagateTargets, \
     AnonymousHyperlinks, IndirectHyperlinks, ExternalTargets, \
     InternalTargets, DanglingReferences, Footnotes
from docutils.parsers.rst import Parser


def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s

totest = {}

totest['tables_of_contents'] = ((PropagateTargets, AnonymousHyperlinks,
                                 IndirectHyperlinks,
                                 ExternalTargets, InternalTargets,
                                 DanglingReferences,
    ), [
["""\
.. _target: http://exammple.org

A reference to a target_.

.. target-notes::
""",
"""\
<document source="test data">
    <target ids="target" names="target" refuri="http://exammple.org">
    <paragraph>
        A reference to a \n\
        <reference name="target" refuri="http://exammple.org">
            target
         \n\
        <footnote_reference auto="1" ids="id2" refid="id1">
        .
    <footnote auto="1" ids="id1" names="TARGET_NOTE:\\ id1">
        <paragraph>
            <reference refuri="http://exammple.org">
                http://exammple.org
"""],
["""\
.. _target: http://exammple.org

A reference to a target_.

.. target-notes:: :class: custom
""",
"""\
<document source="test data">
    <target ids="target" names="target" refuri="http://exammple.org">
    <paragraph>
        A reference to a \n\
        <reference name="target" refuri="http://exammple.org">
            target
        <inline classes="custom">
             \n\
        <footnote_reference auto="1" classes="custom" ids="id2" refid="id1">
        .
    <footnote auto="1" ids="id1" names="TARGET_NOTE:\\ id1">
        <paragraph>
            <reference refuri="http://exammple.org">
                http://exammple.org
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
