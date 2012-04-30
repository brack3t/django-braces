#!/usr/bin/env python

# $Id: test_functional.py 7097 2011-07-20 22:12:25Z milde $
# Author: Lea Wiemann <LeWiemann@gmail.com>
# Copyright: This module has been placed in the public domain.

"""
Perform tests with the data in the functional/ directory.

Read README.txt for details on how this is done.
"""

import sys
import os
import os.path
import shutil
import unittest
import difflib
import DocutilsTestSupport              # must be imported before docutils
import docutils
import docutils.core


datadir = 'functional'
"""The directory to store the data needed for the functional tests."""


def join_path(*args):
    return '/'.join(args) or '.'


class FunctionalTestSuite(DocutilsTestSupport.CustomTestSuite):

    """Test suite containing test cases for all config files."""

    def __init__(self):
        """Process all config files in functional/tests/."""
        DocutilsTestSupport.CustomTestSuite.__init__(self)
        os.chdir(DocutilsTestSupport.testroot)
        self.clear_output_directory()
        self.added = 0
        try:
            for root, dirs, files in os.walk(join_path(datadir, 'tests')):
                # Process all config files among `names` in `dirname`. A config
                # file is a Python file (*.py) which sets several variables.
                for name in files:
                    if name.endswith('.py') and not name.startswith('_'):
                        config_file_full_path = join_path(root, name)
                        self.addTestCase(FunctionalTestCase, 'test', None, None,
                                         id=config_file_full_path,
                                         configfile=config_file_full_path)
                        self.added += 1
        except (AttributeError): # python2.2 does not have os.walk
            os.path.walk(join_path(datadir, 'tests'), self.walker, None)
        assert self.added, 'No functional tests found.'

    def clear_output_directory(self):
        files = os.listdir(os.path.join('functional', 'output'))
        for f in files:
            if f in ('README.txt', '.svn', 'CVS'):
                continue                # don't touch the infrastructure
            path = os.path.join('functional', 'output', f)
            if os.path.isdir(path):
                shutil.rmtree(path)
            else:
                os.remove(path)

    def walker(self, dummy, dirname, names):
        """
        Process all config files among `names` in `dirname`.

        This is a helper function for os.path.walk.  A config file is
        a Python file (*.py) which sets several variables.
        """
        for name in names:
            if name.endswith('.py') and not name.startswith('_'):
                config_file_full_path = join_path(dirname, name)
                self.addTestCase(FunctionalTestCase, 'test', None, None,
                                 id=config_file_full_path,
                                 configfile=config_file_full_path)
                self.added += 1


class FunctionalTestCase(DocutilsTestSupport.CustomTestCase):

    """Test case for one config file."""

    no_expected_template = """\
Cannot find expected output at %(exp)s
If the output in %(out)s
is correct, move it to the expected/ dir and check it in:

  mv %(out)s %(exp)s
  svn add %(exp)s
  svn commit -m "<comment>" %(exp)s"""

    expected_output_differs_template = """\
The expected and actual output differs.
Please compare the expected and actual output files:

  diff %(exp)s %(out)s\n'

If the actual output is correct, please replace the
expected output and check it in:

  mv %(out)s %(exp)s
  svn add %(exp)s
  svn commit -m "<comment>" %(exp)s"""

    def __init__(self, *args, **kwargs):
        """Set self.configfile, pass arguments to parent __init__."""
        self.configfile = kwargs['configfile']
        del kwargs['configfile']
        DocutilsTestSupport.CustomTestCase.__init__(self, *args, **kwargs)

    def shortDescription(self):
        return 'test_functional.py: ' + self.configfile

    def test(self):
        """Process self.configfile."""
        os.chdir(DocutilsTestSupport.testroot)
        # Keyword parameters for publish_file:
        namespace = {}
        # Initialize 'settings_overrides' for test settings scripts,
        # and disable configuration files:
        namespace['settings_overrides'] = {'_disable_config': 1}
        # Read the variables set in the default config file and in
        # the current config file into namespace:
        defaultpy = open(join_path(datadir, 'tests', '_default.py')).read()
        exec(defaultpy, namespace)
        exec(open(self.configfile).read(), namespace)
        # Check for required settings:
        assert 'test_source' in namespace,\
               "No 'test_source' supplied in " + self.configfile
        assert 'test_destination' in namespace,\
               "No 'test_destination' supplied in " + self.configfile
        # Set source_path and destination_path if not given:
        namespace.setdefault('source_path',
                             join_path(datadir, 'input',
                                       namespace['test_source']))
        # Path for actual output:
        namespace.setdefault('destination_path',
                             join_path(datadir, 'output',
                                       namespace['test_destination']))
        # Path for expected output:
        expected_path = join_path(datadir, 'expected',
                                  namespace['test_destination'])
        # shallow copy of namespace to minimize:
        params = namespace.copy()
        # remove unneeded parameters:
        del params['test_source']
        del params['test_destination']
        # Delete private stuff like params['__builtins__']:
        for key in params.keys():
            if key.startswith('_'):
                del params[key]
        # Get output (automatically written to the output/ directory
        # by publish_file):
        output = docutils.core.publish_file(**params)
        # ensure output is unicode
        output_encoding = params.get('output_encoding', 'utf-8')
        if sys.version_info < (3,0):
            try:
                output = output.decode(output_encoding)
            except UnicodeDecodeError:
                # failsafe
                output = output.decode('latin1', 'replace')
        # Normalize line endings:
        output = '\n'.join(output.splitlines())
        # Get the expected output *after* writing the actual output.
        no_expected = self.no_expected_template % {
            'exp': expected_path, 'out': params['destination_path']}
        self.assert_(os.access(expected_path, os.R_OK), no_expected)
        if sys.version_info < (3,0):
            f = open(expected_path, 'r')
        else: # samples are UTF8 encoded. 'rb' leads to errors with Python 3!
            f = open(expected_path, 'r', encoding='utf-8')
        # Normalize line endings:
        expected = '\n'.join(f.read().splitlines())
        f.close()
        if sys.version_info < (3,0):
            try:
                expected = expected.decode(output_encoding)
            except UnicodeDecodeError:
                expected = expected.decode('latin1', 'replace')

        diff = self.expected_output_differs_template % {
            'exp': expected_path, 'out': params['destination_path']}
        try:
            self.assertEquals(output, expected, diff)
        except AssertionError:
            diff = ''.join(difflib.unified_diff(
                expected.splitlines(True), output.splitlines(True),
                expected_path, params['destination_path']))
            if sys.version_info < (3,0):
                diff = diff.encode(sys.stderr.encoding or 'ascii', 'replace')
            print >>sys.stderr, '\n%s:' % (self,)
            print >>sys.stderr, diff
            raise
        # Execute optional function containing extra tests:
        if '_test_more' in namespace:
            namespace['_test_more'](join_path(datadir, 'expected'),
                                    join_path(datadir, 'output'),
                                    self, namespace)


def suite():
    return FunctionalTestSuite()


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
