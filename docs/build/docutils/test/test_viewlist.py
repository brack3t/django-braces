#! /usr/bin/env python

# $Id: test_viewlist.py 6340 2010-06-11 10:31:18Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for the ViewList class from statemachine.py.
"""

import unittest
import sys
import re
from DocutilsTestSupport import statemachine


class ViewListTests(unittest.TestCase):

    a_list = list('abcdefg')
    b_list = list('AEIOU')
    c_list = list('XYZ')

    def setUp(self):
        self.a = statemachine.ViewList(self.a_list, 'a')
        self.b = statemachine.ViewList(self.b_list, 'b')
        self.c = statemachine.ViewList(self.c_list, 'c')

    def test_xitems(self):
        self.assertEqual(list(self.b.xitems()),
                         [('b', 0, 'A'), ('b', 1, 'E'),
                          ('b', 2, 'I'), ('b', 3, 'O'), ('b', 4, 'U')])
        self.assertEqual(list(self.c.xitems()),
                         [('c', 0, 'X'), ('c', 1, 'Y'), ('c', 2, 'Z')])

    def test_lists(self):
        # be compatible to standard lists whenever sensible:
        self.assertEqual(self.a, self.a_list)
        self.assertEqual(str(self.a), str(self.a_list))
        self.assertEqual(self.b, self.b_list)
        self.assertEqual(self.c, self.c_list)
        self.assertEqual(len(self.a), len(self.a_list))
        self.failUnless('d' in self.a) # __contains__
        self.assertEqual([value for value in self.a], self.a_list)
        # get and set values
        self.assertEqual(self.a[2], self.a_list[2])
        a = self.a[:]
        self.assertEqual(a, self.a)
        a[1] = 3
        self.assertEqual(a[1], 3)
        # the `items` list contains the metadata (source/offset tuples)
        self.assertEqual(self.a.items,
                         [('a', i) for (i, v) in enumerate(self.a_list)])

    def test_special_class_methods(self):
        # `repr` returns instantiation expression
        self.assertEqual(repr(self.a), "ViewList(%s, items=%s)" %
                         (repr(self.a_list), repr(self.a.items)))
        # `del` also deletes meta-data:
        del(self.c[1])
        self.assertEqual(list(self.c.xitems()),
                         [('c', 0, 'X'), ('c', 2, 'Z')])
        # operators with extended behaviour
        ab = self.a + self.b
        self.assertEqual(ab, self.a_list + self.b_list)
        self.assertEqual(ab.items, self.a.items + self.b.items)
        aa = self.a * 2
        self.assertEqual(aa, self.a_list * 2)
        self.assertEqual(aa.items, self.a.items * 2)
        self.a += self.b
        self.assertEqual(self.a, self.a_list + self.b_list)
        # self.assertEqual(self.a.items, self.a.items + self.b.items)

    def test_get_slice(self):
        a = self.a[1:-1]
        a_list = self.a_list[1:-1]
        self.assertEqual(a, a_list)
        self.assertEqual(a.items, [('a', i+1) for (i, v) in enumerate(a_list)])
        self.assertEqual(a.parent, self.a)
        # a.pprint()

    def test_set_slice(self):
        a = statemachine.ViewList(self.a[:])
        s = a[2:-2]
        s[2:2] = self.b
        s_list = self.a_list[2:-2]
        s_list[2:2] = self.b_list
        # s.pprint()
        # s[1:4].pprint()
        self.assertEqual(s, s_list)
        self.assertEqual(s, a[2:-2])
        self.assertEqual(s.items, a[2:-2].items)

    def test_del_slice(self):
        a = statemachine.ViewList(self.a[:])
        s = a[2:]
        s_list = self.a_list[2:]
        del s[3:5]
        del s_list[3:5]
        self.assertEqual(s, s_list)
        self.assertEqual(s, a[2:])
        self.assertEqual(s.items, a[2:].items)

    def test_insert(self):
        a_list = self.a_list[:]
        a_list.insert(2, 'Q')
        a_list[4:4] = self.b_list
        a = self.a[:]
        self.assert_(isinstance(a, statemachine.ViewList))
        a.insert(2, 'Q', 'runtime')
        a.insert(4, self.b)
        self.assertEqual(a, a_list)
        self.assertEqual(a.info(2), ('runtime', 0))
        self.assertEqual(a.info(5), ('b', 1))

    def test_append(self):
        a_list = self.a_list[:]
        a_list.append('Q')
        a_list.extend(self.b_list)
        a = statemachine.ViewList(self.a)
        a.append('Q', 'runtime')
        a.append(self.b)
        # a.pprint()
        self.assertEqual(a, a_list)
        self.assertEqual(a.info(len(self.a)), ('runtime', 0))
        self.assertEqual(a.info(-2), ('b', len(self.b) - 2))

    def test_extend(self):
        a_list = self.a_list[:]
        a_list.extend(self.b_list)
        a = statemachine.ViewList(self.a)
        a.extend(self.b)
        self.assertEqual(a, a_list)
        self.assertEqual(a.info(len(self.a) + 1), ('b', 1))
        # a.pprint()

    def test_view(self):
        a = statemachine.ViewList(self.a[:])
        a.insert(4, self.b)
        s = a[2:-2]
        s.insert(5, self.c)
        self.assertEqual(s, a[2:-2])
        self.assertEqual(s.items, a[2:-2].items)
        s.pop()
        self.assertEqual(s, a[2:-2])
        self.assertEqual(s.items, a[2:-2].items)
        s.remove('X')
        self.assertEqual(s, a[2:-2])
        self.assertEqual(s.items, a[2:-2].items)

    def test_trim(self):
        a = statemachine.ViewList(self.a[:])
        s = a[1:-1]
        s.trim_start(1)
        self.assertEquals(a, self.a)
        self.assertEquals(s, a[2:-1])
        s.trim_end(1)
        self.assertEquals(a, self.a)
        self.assertEquals(s, a[2:-2])

    def test_info(self):
        ab = self.a + self.b
        self.assertEqual(ab.info(0), ('a', 0))
        self.assertEqual(ab.info(-1), ('b', len(self.b)-1))
        # report source if index is off the list by one
        self.assertEqual(ab.info(len(ab)), ('b', None))
        # `source` and `offset` methods are based on info
        self.assertEqual(ab.source(-1), 'b')
        self.assertEqual(ab.offset(-1), len(self.b)-1)

    def test_reverse(self):
        c = self.c[:]
        c.reverse()
        self.assertEqual(list(c.xitems()),
                         [('c', 2, 'Z'), ('c', 1, 'Y'), ('c', 0, 'X')])

    def test_sort(self):
        c = self.c[:]
        c.reverse()
        # c.pprint()
        c.sort()
        self.assertEqual(self.c, c)

#         print
#         print a
#         print s
#         print a.items
#         print s.items


class StringList(unittest.TestCase):

    text = """\
This is some
example text.

    Here is some
    indented text.

Unindented text.
"""

    indented_string = """\
        a
      literal
           block"""


    def setUp(self):
        self.a_list = self.text.splitlines(1)
        self.a = statemachine.StringList(self.a_list, 'a')

    def test_trim_left(self):
        s = self.a[3:5]
        s.trim_left(4)
        self.assertEqual(s, [line.lstrip() for line in self.a_list[3:5]])

    def test_get_indented(self):
        self.assertEquals(self.a.get_indented(),
                          ([], 0, 0))
        block = statemachine.StringList(
            statemachine.string2lines(self.indented_string))
        self.assertEquals(block.get_indented(),
                          ([s[6:] for s in block], 6, 1))


if __name__ == '__main__':
    unittest.main()
