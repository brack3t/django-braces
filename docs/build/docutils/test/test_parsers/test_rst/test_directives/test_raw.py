#! /usr/bin/env python

# $Id: test_raw.py 7037 2011-05-19 08:56:27Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for misc.py "raw" directive.
"""

import os.path
import sys
from __init__ import DocutilsTestSupport
from docutils._compat import u_prefix, b

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

mydir = 'test_parsers/test_rst/test_directives/'
raw1 = os.path.join(mydir, 'raw1.txt')
utf_16_file = os.path.join(mydir, 'utf-16.csv')
utf_16_file_rel = DocutilsTestSupport.utils.relative_path(None, utf_16_file)
utf_16_error_str = ("UnicodeDecodeError: 'ascii' codec can't decode byte 0xfe "
                    "in position 0: ordinal not in range(128)")
if sys.version_info < (3,0):
    utf_16_error_str = ("UnicodeError: Unable to decode input data.  "
                        "Tried the following encodings: 'ascii'.\n"
                        "            (%s)" % utf_16_error_str)

totest = {}

totest['raw'] = [
["""\
.. raw:: html

   <span>This is some plain old raw text.</span>
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        <span>This is some plain old raw text.</span>
"""],
["""\
.. raw:: html
   :file: %s
""" % raw1,
"""\
<document source="test data">
    <raw format="html" source="%s" xml:space="preserve">
        <p>This file is used by <tt>test_raw.py</tt>.</p>
""" % DocutilsTestSupport.utils.relative_path(None, raw1)],
["""\
.. raw:: html
   :file: rawfile.html
   :url: http://example.org/
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            The "file" and "url" options may not be simultaneously specified for the "raw" directive.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
               :url: http://example.org/
"""],
["""\
.. raw:: html
   :file: rawfile.html

   <p>Can't have both content and file attribute.</p>
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            "raw" directive may not both specify an external file and have content.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: rawfile.html
            
               <p>Can't have both content and file attribute.</p>
"""],
[r"""
.. raw:: latex html
 
   \[ \sum_{n=1}^\infty \frac{1}{n} \text{ etc.} \]
""",
"""\
<document source="test data">
    <raw format="latex html" xml:space="preserve">
        \\[ \\sum_{n=1}^\\infty \\frac{1}{n} \\text{ etc.} \\]
"""],
["""\
.. raw:: html
   :file: %s
   :encoding: utf-16
""" % utf_16_file_rel,
b("""\
<document source="test data">
    <raw format="html" source="%s" xml:space="preserve">
        "Treat", "Quantity", "Description"
        "Albatr\xb0\xdf", 2.99, "\xa1On a \\u03c3\\u03c4\\u03b9\\u03ba!"
        "Crunchy Frog", 1.49, "If we took the b\xf6nes out, it wouldn\\u2019t be
        crunchy, now would it?"
        "Gannet Ripple", 1.99, "\xbfOn a \\u03c3\\u03c4\\u03b9\\u03ba?"
""" % utf_16_file_rel).decode('raw_unicode_escape')],
["""\
Raw input file is UTF-16-encoded, and is not valid ASCII.

.. raw:: html
   :file: %s
   :encoding: ascii
""" % utf_16_file_rel,
"""\
<document source="test data">
    <paragraph>
        Raw input file is UTF-16-encoded, and is not valid ASCII.
    <system_message level="4" line="3" source="test data" type="SEVERE">
        <paragraph>
            Problem with "raw" directive:
            %s
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: %s
               :encoding: ascii
""" % (utf_16_error_str, utf_16_file_rel)],
[u"""\
.. raw:: html
   :encoding: utf-8

   Should the parser complain becau\xdfe there is no :file:?  BUG?
""",
"""\
<document source="test data">
    <raw format="html" xml:space="preserve">
        Should the parser complain becau\xdfe there is no :file:?  BUG?
"""],
["""\
.. raw:: html
""",
"""\
<document source="test data">
    <system_message level="3" line="1" source="test data" type="ERROR">
        <paragraph>
            Content block expected for the "raw" directive; none found.
        <literal_block xml:space="preserve">
            .. raw:: html
"""],
["""\
.. raw:: html
   :file: non-existent.file
""",
"""\
<document source="test data">
    <system_message level="4" line="1" source="test data" type="SEVERE">
        <paragraph>
            Problems with "raw" directive path:
            IOError: [Errno 2] No such file or directory: %s'non-existent.file'.
        <literal_block xml:space="preserve">
            .. raw:: html
               :file: non-existent.file
""" % u_prefix],
# note that this output is rewritten below for certain python versions
]

# Rewrite tests that depend on the output of IOError as it is
# platform-dependent before python 2.4 for a unicode path.
if sys.version_info < (2, 4):
    # remove the unicode repr u except for py2.3 on windows:
    if not sys.platform.startswith('win') or sys.version_info < (2, 3):
        for i in range(len(totest['raw'])):
            if totest['raw'][i][1].find("u'non-existent.file'") != -1:
                totest['raw'][i][1] = totest['raw'][i][1].replace(
                        "u'non-existent.file'", "'non-existent.file'")


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
