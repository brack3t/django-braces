#!/usr/bin/env python

# $Id: test_buildhtml.py 6370 2010-07-10 06:57:08Z grubert $
# Author: engelbert gruber <grubert@users.sourceforge.net>
# Copyright: This module has been placed in the public domain.

"""
test buildhtml options, because ``--local`` is broken.

Build-HTML Options
------------------
--recurse               Recursively scan subdirectories for files to process.
                        This is the default.
--local                 Do not scan subdirectories for files to process.
--prune=<directory>     Do not process files in <directory>.  This option may
                        be used more than once to specify multiple
                        directories.
--ignore=<patterns>     Recursively ignore files or directories matching any
                        of the given wildcard (shell globbing) patterns
                        (separated by colons).  Default: ".svn:CVS"
--silent                Work silently (no progress messages).  Independent of
                        "--quiet".
"""

import unittest
import os
import re

def process_and_return_filelist(options):
    dirs = []
    files = []
    cin, cout = os.popen4("../buildhtml.py "+options)
    while 1:
        ln = cout.readline()
        if not ln:
            break
        # BUG no colon in filename/path allowed
        item = ln.split(":")[-1].strip()
        if ln.startswith(" "):
            files.append(item)
        else:
            dirs.append(item)
    return (dirs, files)

class BuildHtmlTests(unittest.TestCase):
    tree = ( "_tmp_test_tree",
             "_tmp_test_tree/one.txt", 
             "_tmp_test_tree/two.txt", 
             "_tmp_test_tree/dir1", 
             "_tmp_test_tree/dir1/one.txt", 
             "_tmp_test_tree/dir1/two.txt", 
             "_tmp_test_tree/dir2", 
             "_tmp_test_tree/dir2/one.txt", 
             "_tmp_test_tree/dir2/two.txt", 
             "_tmp_test_tree/dir2/sub", 
             "_tmp_test_tree/dir2/sub/one.txt", 
             "_tmp_test_tree/dir2/sub/two.txt", 
             )

    def setUp(self):
        self.root = os.tempnam()
        os.mkdir(self.root)
        for s in self.tree:
            s = os.path.join(self.root, s)
            if not "." in s:
                os.mkdir(s)
            else:
                open(s, "w").write("dummy")

    def tearDown(self):
        for i in range(len(self.tree) - 1, -1, -1):
            s = os.path.join(self.root, self.tree[i])
            if not "." in s:
                os.rmdir(s)
            else:
                os.remove(s)
        os.rmdir(self.root)

    def test_1(self):
        opts = "--dry-run "+ self.root
        dirs, files = process_and_return_filelist( opts )
        self.assertEquals(files.count("one.txt"), 4)

    def test_local(self):
        opts = "--dry-run --local "+ self.root
        dirs, files = process_and_return_filelist( opts )
        self.assertEquals( len(dirs), 1)
        self.assertEquals( files, [])

if __name__ == '__main__':
    unittest.main()
