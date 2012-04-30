# -*- coding: utf-8 -*-
#! /usr/bin/env python

# $Id: test_utils.py 7007 2011-04-12 09:10:57Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for utils.py.
"""

import unittest
import sys
from DocutilsTestSupport import utils, nodes
try:
    from io import StringIO
except ImportError:    # io is new in Python 2.6
    from StringIO import StringIO


class ReporterTests(unittest.TestCase):

    stream = StringIO()
    reporter = utils.Reporter('test data', 2, 4, stream, 1)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_level0(self):
        sw = self.reporter.system_message(0, 'debug output')
        self.assertEquals(sw.pformat(), """\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        debug output
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (DEBUG/0) debug output\n')

    def test_level1(self):
        sw = self.reporter.system_message(1, 'a little reminder')
        self.assertEquals(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        a little reminder
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_level2(self):
        sw = self.reporter.system_message(2, 'a warning')
        self.assertEquals(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (WARNING/2) a warning\n')

    def test_level3(self):
        sw = self.reporter.system_message(3, 'an error')
        self.assertEquals(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEquals(self.stream.getvalue(),
                          'test data:: (ERROR/3) an error\n')

    def test_level4(self):
        self.assertRaises(utils.SystemMessage, self.reporter.system_message, 4,
                          'a severe error, raises an exception')
        self.assertEquals(self.stream.getvalue(), 'test data:: (SEVERE/4) '
                          'a severe error, raises an exception\n')


    def test_unicode_message(self):
        sw = self.reporter.system_message(0, u'mesidʒ')
        self.assertEquals(sw.pformat(), u"""\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        mesidʒ
""")

    def test_unicode_message_from_exception(self):
        """Workaround for Python < 2.6 bug:
        unicode(<exception instance>) uses __str__
        and hence fails with unicode message"""
        try:
            raise Exception(u'mesidʒ')
        except Exception, err:
            sw = self.reporter.system_message(0, err)
            self.assertEquals(sw.pformat(), u"""\
<system_message level="0" source="test data" type="DEBUG">
    <paragraph>
        mesidʒ
""")

class QuietReporterTests(unittest.TestCase):

    stream = StringIO()
    reporter = utils.Reporter('test data', 5, 5, stream, 0)

    def setUp(self):
        self.stream.seek(0)
        self.stream.truncate()

    def test_debug(self):
        sw = self.reporter.debug('a debug message')
        # None because debug is disabled.
        self.assertEquals(sw, None)
        self.assertEquals(self.stream.getvalue(), '')

    def test_info(self):
        sw = self.reporter.info('an informational message')
        self.assertEquals(sw.pformat(), """\
<system_message level="1" source="test data" type="INFO">
    <paragraph>
        an informational message
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_warning(self):
        sw = self.reporter.warning('a warning')
        self.assertEquals(sw.pformat(), """\
<system_message level="2" source="test data" type="WARNING">
    <paragraph>
        a warning
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_error(self):
        sw = self.reporter.error('an error')
        self.assertEquals(sw.pformat(), """\
<system_message level="3" source="test data" type="ERROR">
    <paragraph>
        an error
""")
        self.assertEquals(self.stream.getvalue(), '')

    def test_severe(self):
        sw = self.reporter.severe('a severe error')
        self.assertEquals(sw.pformat(), """\
<system_message level="4" source="test data" type="SEVERE">
    <paragraph>
        a severe error
""")
        self.assertEquals(self.stream.getvalue(), '')


class NameValueTests(unittest.TestCase):

    def test_extract_name_value(self):
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          '=hello')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello=')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="something')
        self.assertRaises(utils.NameValueError, utils.extract_name_value,
                          'hello="something"else')
        output = utils.extract_name_value(
              """att1=val1 att2=val2 att3="value number '3'" att4=val4""")
        self.assertEquals(output, [('att1', 'val1'), ('att2', 'val2'),
                                   ('att3', "value number '3'"),
                                   ('att4', 'val4')])


class ExtensionOptionTests(unittest.TestCase):

    optionspec = {'a': int, 'bbb': float, 'cdef': (lambda x: x),
                  'empty': (lambda x: x)}

    def test_assemble_option_dict(self):
        input = utils.extract_name_value('a=1 bbb=2.0 cdef=hol%s' % chr(224))
        self.assertEquals(
              utils.assemble_option_dict(input, self.optionspec),
              {'a': 1, 'bbb': 2.0, 'cdef': ('hol%s' % chr(224))})
        input = utils.extract_name_value('a=1 b=2.0 c=hol%s' % chr(224))
        self.assertRaises(KeyError, utils.assemble_option_dict,
                          input, self.optionspec)
        input = utils.extract_name_value('a=1 bbb=two cdef=hol%s' % chr(224))
        self.assertRaises(ValueError, utils.assemble_option_dict,
                          input, self.optionspec)

    def test_extract_extension_options(self):
        field_list = nodes.field_list()
        field_list += nodes.field(
              '', nodes.field_name('', 'a'),
              nodes.field_body('', nodes.paragraph('', '1')))
        field_list += nodes.field(
              '', nodes.field_name('', 'bbb'),
              nodes.field_body('', nodes.paragraph('', '2.0')))
        field_list += nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', u'hol\u00e0')))
        field_list += nodes.field(
              '', nodes.field_name('', 'empty'), nodes.field_body())
        self.assertEquals(
              utils.extract_extension_options(field_list, self.optionspec),
              {'a': 1, 'bbb': 2.0,
               'cdef': u'hol\u00e0',
               'empty': None})
        self.assertRaises(KeyError, utils.extract_extension_options,
                          field_list, {})
        field_list += nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'one'),
                               nodes.paragraph('', 'two')))
        self.assertRaises(utils.BadOptionDataError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef bad'),
              nodes.field_body('', nodes.paragraph('', 'no arguments')))
        self.assertRaises(utils.BadOptionError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-1] = nodes.field(
              '', nodes.field_name('', 'cdef'),
              nodes.field_body('', nodes.paragraph('', 'duplicate')))
        self.assertRaises(utils.DuplicateOptionError,
                          utils.extract_extension_options,
                          field_list, self.optionspec)
        field_list[-2] = nodes.field(
              '', nodes.field_name('', 'unkown'),
              nodes.field_body('', nodes.paragraph('', 'unknown')))
        self.assertRaises(KeyError, utils.extract_extension_options,
                          field_list, self.optionspec)


class HelperFunctionsTests(unittest.TestCase):

    def test_normalize_language_tag(self):
        self.assertEquals(utils.normalize_language_tag('de'), ['de'])
        self.assertEquals(utils.normalize_language_tag('de-AT'),
                          ['de_at', 'de'])
        self.assertEquals(utils.normalize_language_tag('de-AT-1901'),
                          ['de_at_1901', 'de_at', 'de_1901', 'de'])
        self.assertEquals(utils.normalize_language_tag('de-AT-1901-frak'),
                          ['de_at_1901_frak', 'de_at_1901', 'de_at_frak',
                          'de_1901_frak', 'de_at', 'de_1901', 'de_frak', 'de'])

    def test_column_width(self):
        self.assertEquals(utils.column_width(u'de'), 2)
        self.assertEquals(utils.column_width(u'dâ'), 2) # pre-composed
        self.assertEquals(utils.column_width(u'dâ'), 2) # combining


if __name__ == '__main__':
    unittest.main()
