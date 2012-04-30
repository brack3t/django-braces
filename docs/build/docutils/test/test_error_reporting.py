#! /usr/bin/env python
# .. coding: utf8
# $Id: test_error_reporting.py 7097 2011-07-20 22:12:25Z milde $
# Author: Günter Milde <milde@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
Test `EnvironmentError` reporting.

In some locales, the `errstr` argument of IOError and OSError contains
non-ASCII chars.

In Python 2, converting an exception instance to `str` or `unicode`
might fail, with non-ASCII chars in arguments and the default encoding
and errors ('ascii', 'strict').

Therefore, Docutils must not use string interpolation with exception
instances like, e.g., ::

  try:
    something
  except IOError, error:
    print 'Found %s' % error

unless the minimal required Python version has this problem fixed.
"""

import unittest
import sys, os
import codecs
try: # from standard library module `io`
    from io import StringIO, BytesIO
except ImportError: # new in Python 2.6
    from StringIO import StringIO
    BytesIO = StringIO

import DocutilsTestSupport              # must be imported before docutils
from docutils import core, parsers, frontend, utils
from docutils.error_reporting import SafeString, ErrorString, ErrorOutput
from docutils._compat import b, bytes

oldlocale = None
if sys.version_info < (3,0): # problems solved in py3k
    try:
        import locale # module missing in Jython
        oldlocale = locale.getlocale()
        # Why does getlocale return the defaultlocale in Python 3.2 ????
        # oldlocale = (None, None) # test suite runs without locale
    except ImportError:
        print ('cannot test error reporting with problematic locales,\n'
            '`import locale` failed.')


# locales confirmed to use non-ASCII chars in the IOError message
# for a missing file (https://bugs.gentoo.org/show_bug.cgi?id=349101)
# TODO: add more confirmed problematic locales
problematic_locales = ['cs_CZ', 'cs_CZ.UTF8',
                       'el_GR', 'el_GR.UTF-8',
                       # 'fr_FR.UTF-8', # only OSError
                       'ja_JP.UTF-8',
                       'ru_RU', 'ru_RU.KOI8-R',
                       'ru_RU.UTF-8',
                       '',  # default locale: might be non-problematic
                       ]

if oldlocale is not None:
    # find a supported problematic locale:
    for testlocale in problematic_locales:
        try:
            locale.setlocale(locale.LC_ALL, testlocale)
        except locale.Error:
            testlocale = None
        else:
            break
    locale.setlocale(locale.LC_ALL, oldlocale) # reset
else:
    testlocale = None

class SafeStringTests(unittest.TestCase):
    # the error message in EnvironmentError instances comes from the OS
    # and in some locales (e.g. ru_RU), contains high bit chars.
    # -> see the test in test_error_reporting.py

    # test data:
    bs = b('\xfc')     # unicode(bs) fails, str(bs) in Python 3 return repr()
    us = u'\xfc'       # bytes(us) fails; str(us) fails in Python 2
    be = Exception(bs) # unicode(be) fails
    ue = Exception(us) # bytes(ue) fails, str(ue) fails in Python 2;
                       # unicode(ue) fails in Python < 2.6 (issue2517_)
                       # .. _issue2517: http://bugs.python.org/issue2517
    # wrapped test data:
    wbs = SafeString(bs)
    wus = SafeString(us)
    wbe = SafeString(be)
    wue = SafeString(ue)

    def test_7bit(self):
        # wrapping (not required with 7-bit chars) must not change the
        # result of conversions:
        bs7 = b('foo')
        us7 = u'foo'
        be7 = Exception(bs7)
        ue7 = Exception(us7)
        self.assertEqual(str(42), str(SafeString(42)))
        self.assertEqual(str(bs7), str(SafeString(bs7)))
        self.assertEqual(str(us7), str(SafeString(us7)))
        self.assertEqual(str(be7), str(SafeString(be7)))
        self.assertEqual(str(ue7), str(SafeString(ue7)))
        self.assertEqual(unicode(7), unicode(SafeString(7)))
        self.assertEqual(unicode(bs7), unicode(SafeString(bs7)))
        self.assertEqual(unicode(us7), unicode(SafeString(us7)))
        self.assertEqual(unicode(be7), unicode(SafeString(be7)))
        self.assertEqual(unicode(ue7), unicode(SafeString(ue7)))

    def test_ustr(self):
        """Test conversion to a unicode-string."""
        # unicode(self.bs) fails
        self.assertEqual(unicode, type(unicode(self.wbs)))
        self.assertEqual(unicode(self.us), unicode(self.wus))
        # unicode(self.be) fails
        self.assertEqual(unicode, type(unicode(self.wbe)))
        # unicode(ue) fails in Python < 2.6 (issue2517_)
        self.assertEqual(unicode, type(unicode(self.wue)))
        self.assertEqual(self.us, unicode(self.wue))

    def test_str(self):
        """Test conversion to a string (bytes in Python 2, unicode in Python 3)."""
        self.assertEqual(str(self.bs), str(self.wbs))
        self.assertEqual(str(self.be), str(self.be))
        # str(us) fails in Python 2
        self.assertEqual(str, type(str(self.wus)))
        # str(ue) fails in Python 2
        self.assertEqual(str, type(str(self.wue)))


class ErrorStringTests(unittest.TestCase):
    bs = b('\xfc')     # unicode(bs) fails, str(bs) in Python 3 return repr()
    us = u'\xfc'       # bytes(us) fails; str(us) fails in Python 2

    def test_str(self):
        self.assertEqual('Exception: spam',
                         str(ErrorString(Exception('spam'))))
        self.assertEqual('IndexError: '+str(self.bs),
                         str(ErrorString(IndexError(self.bs))))
        self.assertEqual('ImportError: %s' % SafeString(self.us),
                         str(ErrorString(ImportError(self.us))))

    def test_unicode(self):
        self.assertEqual(u'Exception: spam',
                         unicode(ErrorString(Exception(u'spam'))))
        self.assertEqual(u'IndexError: '+self.us,
                         unicode(ErrorString(IndexError(self.us))))
        self.assertEqual(u'ImportError: %s' % SafeString(self.bs),
                         unicode(ErrorString(ImportError(self.bs))))


# ErrorOutput tests
# -----------------

# Stub: Buffer with 'strict' auto-conversion of input to byte string:
class BBuf(BytesIO, object):
    def write(self, data):
        if isinstance(data, unicode):
            data.encode('ascii', 'strict')
        super(BBuf, self).write(data)

# Stub: Buffer expecting unicode string:
class UBuf(StringIO, object):
    def write(self, data):
        # emulate Python 3 handling of stdout, stderr
        if isinstance(data, bytes):
            raise TypeError('must be unicode, not bytes')
        super(UBuf, self).write(data)

class ErrorOutputTests(unittest.TestCase):
    def test_defaults(self):
        e = ErrorOutput()
        self.assertEquals(e.stream, sys.stderr)

    def test_bbuf(self):
        buf = BBuf() # buffer storing byte string
        e = ErrorOutput(buf, encoding='ascii')
        # write byte-string as-is
        e.write(b('b\xfc'))
        self.assertEquals(buf.getvalue(), b('b\xfc'))
        # encode unicode data with backslashescape fallback replacement:
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc'))
        # handle Exceptions with Unicode string args
        # unicode(Exception(u'e\xfc')) # fails in Python < 2.6
        e.write(AttributeError(u' e\xfc'))
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc e\\xfc'))
        # encode with `encoding` attribute
        e.encoding = 'utf8'
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), b('b\xfc u\\xfc e\\xfc u\xc3\xbc'))

    def test_ubuf(self):
        buf = UBuf() # buffer only accepting unicode string
        # decode of binary strings
        e = ErrorOutput(buf, encoding='ascii')
        e.write(b('b\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd') # use REPLACEMENT CHARACTER
        # write Unicode string and Exceptions with Unicode args
        e.write(u' u\xfc')
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc')
        e.write(AttributeError(u' e\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc e\xfc')
        # decode with `encoding` attribute
        e.encoding = 'latin1'
        e.write(b(' b\xfc'))
        self.assertEquals(buf.getvalue(), u'b\ufffd u\xfc e\xfc b\xfc')



class SafeStringTests_locale(unittest.TestCase):
    """
    Test docutils.SafeString with 'problematic' locales.

    The error message in `EnvironmentError` instances comes from the OS
    and in some locales (e.g. ru_RU), contains high bit chars.
    """
    if testlocale:
        locale.setlocale(locale.LC_ALL, testlocale)
    # test data:
    bs = b('\xfc')
    us = u'\xfc'
    try:
        open(b('\xfc'))
    except IOError, e: # in Python 3 the name for the exception instance
        bioe = e       # is local to the except clause
    try:
        open(u'\xfc')
    except IOError, e:
        uioe = e
    except UnicodeEncodeError:
        try:
            open(u'\xfc'.encode(sys.getfilesystemencoding(), 'replace'))
        except IOError, e:
            uioe = e
    try:
        os.chdir(b('\xfc'))
    except OSError, e:
        bose = e
    try:
        os.chdir(u'\xfc')
    except OSError, e:
        uose = e
    except UnicodeEncodeError:
        try:
            os.chdir(u'\xfc'.encode(sys.getfilesystemencoding(), 'replace'))
        except OSError, e:
            uose = e
    # wrapped test data:
    wbioe = SafeString(bioe)
    wuioe = SafeString(uioe)
    wbose = SafeString(bose)
    wuose = SafeString(uose)
    # reset locale
    if testlocale:
        locale.setlocale(locale.LC_ALL, oldlocale)

    def test_ustr(self):
        """Test conversion to a unicode-string."""
        # unicode(bioe) fails with e.g. 'ru_RU.utf8' locale
        self.assertEqual(unicode, type(unicode(self.wbioe)))
        self.assertEqual(unicode, type(unicode(self.wuioe)))
        self.assertEqual(unicode, type(unicode(self.wbose)))
        self.assertEqual(unicode, type(unicode(self.wuose)))

    def test_str(self):
        """Test conversion to a string (bytes in Python 2, unicode in Python 3)."""
        self.assertEqual(str(self.bioe), str(self.wbioe))
        self.assertEqual(str(self.uioe), str(self.wuioe))
        self.assertEqual(str(self.bose), str(self.wbose))
        self.assertEqual(str(self.uose), str(self.wuose))



class ErrorReportingTests(unittest.TestCase):
    """
    Test cases where error reporting can go wrong.

    Do not test the exact output (as this varies with the locale), just
    ensure that the correct exception is thrown.
    """

    # These tests fail with a 'problematic locale' and
    # (revision < 7035) and Python-2.

    parser = parsers.rst.Parser()
    """Parser shared by all ParserTestCases."""

    option_parser = frontend.OptionParser(components=(parsers.rst.Parser,))
    settings = option_parser.get_default_values()
    settings.report_level = 1
    settings.halt_level = 1
    settings.warning_stream = ''
    document = utils.new_document('test data', settings)

    def setUp(self):
        if testlocale:
            locale.setlocale(locale.LC_ALL, testlocale)

    def tearDown(self):
        if testlocale:
            locale.setlocale(locale.LC_ALL, oldlocale)

    def test_include(self):
        source = ('.. include:: bogus.txt')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_raw_file(self):
        source = ('.. raw:: html\n'
                  '   :file: bogus.html\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_raw_url(self):
        source = ('.. raw:: html\n'
                  '   :url: http://bogus.html\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_csv_table(self):
        source = ('.. csv-table:: external file\n'
                  '   :file: bogus.csv\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

    def test_csv_table_url(self):
        source = ('.. csv-table:: external URL\n'
                  '   :url: ftp://bogus.csv\n')
        self.assertRaises(utils.SystemMessage,
                          self.parser.parse, source, self.document)

if __name__ == '__main__':
    unittest.main()
