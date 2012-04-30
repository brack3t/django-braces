#! /usr/bin/env python

# $Id: test_writer_aux.py 5174 2007-05-31 00:01:52Z wiemann $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Test module for writer_aux transforms.
"""

from __init__ import DocutilsTestSupport # must be imported before docutils
from docutils.transforms import writer_aux
from docutils.parsers.rst import Parser

def suite():
    parser = Parser()
    s = DocutilsTestSupport.TransformTestSuite(parser)
    s.generateTests(totest)
    return s


totest = {}

totest['compound'] = ((writer_aux.Compound,), [
["""\
.. class:: compound

.. compound::

   .. class:: paragraph1

   Paragraph 1.

   .. class:: paragraph2

   Paragraph 2.

       Block quote.
""",
"""\
<document source="test data">
    <paragraph classes="paragraph1 compound">
        Paragraph 1.
    <paragraph classes="paragraph2 continued">
        Paragraph 2.
    <block_quote classes="continued">
        <paragraph>
            Block quote.
"""],
])

totest['admonitions'] = ((writer_aux.Admonitions,), [
["""\
.. note::

   These are the note contents.

   Another paragraph.
""",
"""\
<document source="test data">
    <admonition classes="note">
        <title>
            Note
        <paragraph>
            These are the note contents.
        <paragraph>
            Another paragraph.
"""],
["""\
.. admonition:: Generic

   Admonitions contents...
""",
"""\
<document source="test data">
    <admonition classes="admonition-generic admonition">
        <title>
            Generic
        <paragraph>
            Admonitions contents...
"""],
])


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
