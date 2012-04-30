#! /usr/bin/env python

# $Id: test_traversals.py 4641 2006-06-28 16:27:55Z blais $
# Author: Martin Blais <blais@furius.ca>
# Copyright: This module has been placed in the public domain.

"""
Test module for traversals.
"""

import unittest
import DocutilsTestSupport              # must be imported before docutils
from docutils import nodes, core, io, utils, writers
from docutils.writers.null import Writer as NullWriter
import docutils



stop_traversal_input = '''
==================
   Train Travel
==================

Happily, happily going by train.

.. attention:: Attention, attention.  This is a public annoucement.  
               You must get off the train now.

KaZoom! Train crashes.

- Told ya!!!  Get off the train next time.

'''

class AttentiveVisitor(nodes.SparseNodeVisitor):

    def visit_attention(self, node):
        raise nodes.StopTraversal

    def visit_bullet_list(self, node):
        raise RuntimeError("It's too late for attention, "
                           "more discipline is needed!.")

class AttentiveWriter(writers.Writer):

    def translate(self):
        self.visitor = visitor = AttentiveVisitor(self.document)

        # Test both kinds of traversals.
        self.document.walkabout(visitor)
        self.document.walk(visitor)

class StopTraversalTests(unittest.TestCase, docutils.SettingsSpec):
    """
    Test interrupting the visitor during traversal.  In this test we stop it
    when we reach an attention node.
    """
    def test_stop_traversal(self):
        # Load some document tree in memory.
        doctree = docutils.core.publish_doctree(
            source=stop_traversal_input,
            reader_name='standalone',
            parser_name='restructuredtext',
            settings_spec=self)
        self.assert_(isinstance(doctree, nodes.document))

        parts = docutils.core.publish_parts(
           reader_name='doctree', source_class=docutils.io.DocTreeInput,
           source=doctree, source_path='test',
           writer=AttentiveWriter())


if __name__ == '__main__':
    unittest.main()

