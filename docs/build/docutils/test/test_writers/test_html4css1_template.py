#!/usr/bin/env python

# $Id: test_html4css1_template.py 6379 2010-07-19 16:07:19Z goodger $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for the HTML writer.
"""

import os
from __init__ import DocutilsTestSupport


def suite():
    settings = {'template': os.path.join(DocutilsTestSupport.testroot,
                                         'data', 'full-template.txt'),
                'embed_stylesheet': 0,}
    s = DocutilsTestSupport.PublishTestSuite('html', suite_settings=settings)
    s.generateTests(totest)
    return s

totest = {}

totest['template'] = [
["""\
================
 Document Title
================
----------
 Subtitle
----------

:Author: Me

.. footer:: footer text

Section
=======

Some text.
""",
r'''head_prefix = """\
<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils %(version)s: http://docutils.sourceforge.net/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


stylesheet = """\
<link rel="stylesheet" href="../docutils/writers/html4css1/html4css1.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<div class="document" id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


docinfo = """\
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>"""


body = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


body_suffix = """\
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>
</body>
</html>"""


head_prefix = """\
<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>"""


head = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils %(version)s: http://docutils.sourceforge.net/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


stylesheet = """\
<link rel="stylesheet" href="../docutils/writers/html4css1/html4css1.css" type="text/css" />"""


body_prefix = """\
</head>
<body>
<div class="document" id="document-title">"""


body_pre_docinfo = """\
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


docinfo = """\
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>"""


body = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


body_suffix = """\
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>
</body>
</html>"""


title = """\
Document Title"""


subtitle = """\
Subtitle"""


header = """\
"""


footer = """\
<div class="footer">
<hr class="footer" />
footer text
</div>"""


meta = """\
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="Docutils %(version)s: http://docutils.sourceforge.net/" />
<meta name="author" content="Me" />"""


fragment = """\
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>"""


html_prolog = """\
<?xml version="1.0" encoding="%%s" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">"""


html_head = """\
<meta http-equiv="Content-Type" content="text/html; charset=%%s" />
<meta name="generator" content="Docutils %(version)s: http://docutils.sourceforge.net/" />
<title>Document Title</title>
<meta name="author" content="Me" />"""


html_title = """\
<h1 class="title">Document Title</h1>"""


html_subtitle = """\
<h2 class="subtitle" id="subtitle">Subtitle</h2>"""


html_body = """\
<div class="document" id="document-title">
<h1 class="title">Document Title</h1>
<h2 class="subtitle" id="subtitle">Subtitle</h2>
<table class="docinfo" frame="void" rules="none">
<col class="docinfo-name" />
<col class="docinfo-content" />
<tbody valign="top">
<tr><th class="docinfo-name">Author:</th>
<td>Me</td></tr>
</tbody>
</table>
<div class="section" id="section">
<h1>Section</h1>
<p>Some text.</p>
</div>
</div>
<div class="footer">
<hr class="footer" />
footer text
</div>"""
''' % {'version': DocutilsTestSupport.docutils.__version__}]
]

if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
