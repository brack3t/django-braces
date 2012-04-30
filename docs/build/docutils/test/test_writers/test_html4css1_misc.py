#! /usr/bin/env python

# $Id: test_html4css1_misc.py 5889 2009-04-01 20:00:21Z gbrandl $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Miscellaneous HTML writer tests.
"""

from __init__ import DocutilsTestSupport
from docutils import core
from docutils._compat import b


class EncodingTestCase(DocutilsTestSupport.StandardTestCase):

    def test_xmlcharrefreplace(self):
        # Test that xmlcharrefreplace is the default output encoding
        # error handler.
        settings_overrides={
            'output_encoding': 'latin1',
            'stylesheet': '',
            '_disable_config': 1,}
        result = core.publish_string(
            b('EUR = \xe2\x82\xac'), writer_name='html4css1',
            settings_overrides=settings_overrides)
        # Encoding a euro sign with latin1 doesn't work, so the
        # xmlcharrefreplace handler is used.
        self.assert_(result.find(b('EUR = &#8364;')) != -1)


if __name__ == '__main__':
    import unittest
    unittest.main()
