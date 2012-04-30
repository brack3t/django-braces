#! /usr/bin/env python
# -*- coding: utf-8 -*-

# $Id: test_nodes.py 6351 2010-07-03 14:19:09Z gbrandl $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Test module for nodes.py.
"""

import sys
import unittest
import types
import DocutilsTestSupport              # must be imported before docutils
from DocutilsTestSupport import nodes, utils
from docutils._compat import b

debug = 0


class TextTests(unittest.TestCase):

    def setUp(self):
        self.text = nodes.Text('Line 1.\nLine 2.')
        self.unicode_text = nodes.Text(u'Möhren')
        self.longtext = nodes.Text('Mary had a little lamb whose '
                                   'fleece was white as snow and '
                                   'everwhere that Mary went the '
                                   'lamb was sure to go.')

    def test_repr(self):
        self.assertEquals(repr(self.text), r"<#text: 'Line 1.\nLine 2.'>")
        self.assertEquals(self.text.shortrepr(),
                          r"<#text: 'Line 1.\nLine 2.'>")

    def test_str(self):
        self.assertEquals(str(self.text), 'Line 1.\nLine 2.')

    def test_unicode(self):
        self.assertEquals(unicode(self.unicode_text), u'Möhren')
        self.assertEquals(str(self.unicode_text), 'M\xf6hren')

    def test_astext(self):
        self.assertEquals(self.text.astext(), 'Line 1.\nLine 2.')

    def test_pformat(self):
        self.assertEquals(self.text.pformat(), 'Line 1.\nLine 2.\n')

    def test_asciirestriction(self):
        if sys.version_info < (3,):
            self.assertRaises(UnicodeDecodeError, nodes.Text,
                              b('hol%s' % chr(224)))
        else:
            # no bytes at all allowed
            self.assertRaises(TypeError, nodes.Text, b('hol'))

    def test_longrepr(self):
        self.assertEquals(repr(self.longtext), r"<#text: 'Mary had a "
                          r"little lamb whose fleece was white as snow "
                          r"and everwh ...'>")
        self.assertEquals(self.longtext.shortrepr(),
                          r"<#text: 'Mary had a lit ...'>")

class ElementTests(unittest.TestCase):

    def test_empty(self):
        element = nodes.Element()
        self.assertEquals(repr(element), '<Element: >')
        self.assertEquals(str(element), '<Element/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element/>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEquals(repr(element), '<Element: >')
        self.assertEquals(str(element), '<Element attr="1"/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element attr="1"/>')
        dom.unlink()
        self.assertEquals(element.pformat(), '<Element attr="1">\n')
        del element['attr']
        element['mark'] = u'\u2022'
        self.assertEquals(repr(element), '<Element: >')
        if sys.version_info < (3,):
            self.assertEquals(str(element), '<Element mark="\\u2022"/>')
        else:
            self.assertEquals(str(element), '<Element mark="\u2022"/>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), u'<Element mark="\u2022"/>')
        dom.unlink()

    def test_withtext(self):
        element = nodes.Element('text\nmore', nodes.Text('text\nmore'))
        self.assertEquals(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEquals(str(element), '<Element>text\nmore</Element>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(), '<Element>text\nmore</Element>')
        dom.unlink()
        element['attr'] = '1'
        self.assertEquals(repr(element), r"<Element: <#text: 'text\nmore'>>")
        self.assertEquals(str(element),
                          '<Element attr="1">text\nmore</Element>')
        dom = element.asdom()
        self.assertEquals(dom.toxml(),
                          '<Element attr="1">text\nmore</Element>')
        dom.unlink()
        self.assertEquals(element.pformat(),
                          '<Element attr="1">\n    text\n    more\n')

    def test_clear(self):
        element = nodes.Element()
        element += nodes.Element()
        self.assert_(len(element))
        element.clear()
        self.assert_(not len(element))

    def test_normal_attributes(self):
        element = nodes.Element()
        self.assert_('foo' not in element)
        self.assertRaises(KeyError, element.__getitem__, 'foo')
        element['foo'] = 'sometext'
        self.assertEquals(element['foo'], 'sometext')
        del element['foo']
        self.assertRaises(KeyError, element.__getitem__, 'foo')

    def test_default_attributes(self):
        element = nodes.Element()
        self.assertEquals(element['ids'], [])
        self.assertEquals(element.non_default_attributes(), {})
        self.assert_(not element.is_not_default('ids'))
        self.assert_(element['ids'] is not nodes.Element()['ids'])
        element['ids'].append('someid')
        self.assertEquals(element['ids'], ['someid'])
        self.assertEquals(element.non_default_attributes(),
                          {'ids': ['someid']})
        self.assert_(element.is_not_default('ids'))

    def test_update_basic_atts(self):
        element1 = nodes.Element(ids=['foo', 'bar'], test=['test1'])
        element2 = nodes.Element(ids=['baz', 'qux'], test=['test2'])
        element1.update_basic_atts(element2)
        # 'ids' are appended because 'ids' is a basic attribute.
        self.assertEquals(element1['ids'], ['foo', 'bar', 'baz', 'qux'])
        # 'test' is not overwritten because it is not a basic attribute.
        self.assertEquals(element1['test'], ['test1'])

    def test_replace_self(self):
        parent = nodes.Element(ids=['parent'])
        child1 = nodes.Element(ids=['child1'])
        grandchild = nodes.Element(ids=['grandchild'])
        child1 += grandchild
        child2 = nodes.Element(ids=['child2'])
        twins = [nodes.Element(ids=['twin%s' % i]) for i in (1, 2)]
        child2 += twins
        child3 = nodes.Element(ids=['child3'])
        child4 = nodes.Element(ids=['child4'])
        parent += [child1, child2, child3, child4]
        self.assertEquals(parent.pformat(), """\
<Element ids="parent">
    <Element ids="child1">
        <Element ids="grandchild">
    <Element ids="child2">
        <Element ids="twin1">
        <Element ids="twin2">
    <Element ids="child3">
    <Element ids="child4">
""")
        # Replace child1 with the grandchild.
        child1.replace_self(child1[0])
        self.assertEquals(parent[0], grandchild)
        # Assert that 'ids' have been updated.
        self.assertEquals(grandchild['ids'], ['grandchild', 'child1'])
        # Replace child2 with its children.
        child2.replace_self(child2[:])
        self.assertEquals(parent[1:3], twins)
        # Assert that 'ids' have been propagated to first child.
        self.assertEquals(twins[0]['ids'], ['twin1', 'child2'])
        self.assertEquals(twins[1]['ids'], ['twin2'])
        # Replace child3 with new child.
        newchild = nodes.Element(ids=['newchild'])
        child3.replace_self(newchild)
        self.assertEquals(parent[3], newchild)
        self.assertEquals(newchild['ids'], ['newchild', 'child3'])
        # Crazy but possible case: Substitute child4 for itself.
        child4.replace_self(child4)
        # Make sure the 'child4' ID hasn't been duplicated.
        self.assertEquals(child4['ids'], ['child4'])
        self.assertEquals(len(parent), 5)

    def test_unicode(self):
        node = nodes.Element(u'Möhren', nodes.Text(u'Möhren', u'Möhren'))
        self.assertEquals(unicode(node), u'<Element>Möhren</Element>')


class MiscTests(unittest.TestCase):

    def test_node_class_names(self):
        node_class_names = []
        for x in dir(nodes):
            c = getattr(nodes, x)
            if isinstance(c, (type, types.ClassType)) and \
                   issubclass(c, nodes.Node) and len(c.__bases__) > 1:
                node_class_names.append(x)
        node_class_names.sort()
        nodes.node_class_names.sort()
        self.assertEquals(node_class_names, nodes.node_class_names)

    ids = [(u'a', 'a'), ('A', 'a'), ('', ''), ('a b \n c', 'a-b-c'),
           ('a.b.c', 'a-b-c'), (' - a - b - c - ', 'a-b-c'), (' - ', ''),
           (u'\u2020\u2066', ''), (u'a \xa7 b \u2020 c', 'a-b-c'),
           ('1', ''), ('1abc', 'abc'),
          ]
    ids_unicode_all = [
            (u'\u00f8 o with stroke', 'o-o-with-stroke'),
            (u'\u0111 d with stroke', 'd-d-with-stroke'),
            (u'\u0127 h with stroke', 'h-h-with-stroke'),
            (u'\u0131 dotless i', 'i-dotless-i'),
            (u'\u0142 l with stroke', 'l-l-with-stroke'),
            (u'\u0167 t with stroke', 't-t-with-stroke'),
           # From Latin Extended-B
            (u'\u0180 b with stroke', 'b-b-with-stroke'),
            (u'\u0183 b with topbar', 'b-b-with-topbar'),
            (u'\u0188 c with hook', 'c-c-with-hook'),
            (u'\u018c d with topbar', 'd-d-with-topbar'),
            (u'\u0192 f with hook', 'f-f-with-hook'),
            (u'\u0199 k with hook', 'k-k-with-hook'),
            (u'\u019a l with bar', 'l-l-with-bar'),
            (u'\u019e n with long right leg', 'n-n-with-long-right-leg'),
            (u'\u01a5 p with hook', 'p-p-with-hook'),
            (u'\u01ab t with palatal hook', 't-t-with-palatal-hook'),
            (u'\u01ad t with hook', 't-t-with-hook'),
            (u'\u01b4 y with hook', 'y-y-with-hook'),
            (u'\u01b6 z with stroke', 'z-z-with-stroke'),
            (u'\u01e5 g with stroke', 'g-g-with-stroke'),
            (u'\u0225 z with hook', 'z-z-with-hook'),
            (u'\u0234 l with curl', 'l-l-with-curl'),
            (u'\u0235 n with curl', 'n-n-with-curl'),
            (u'\u0236 t with curl', 't-t-with-curl'),
            (u'\u0237 dotless j', 'j-dotless-j'),
            (u'\u023c c with stroke', 'c-c-with-stroke'),
            (u'\u023f s with swash tail', 's-s-with-swash-tail'),
            (u'\u0240 z with swash tail', 'z-z-with-swash-tail'),
            (u'\u0247 e with stroke', 'e-e-with-stroke'),
            (u'\u0249 j with stroke', 'j-j-with-stroke'),
            (u'\u024b q with hook tail', 'q-q-with-hook-tail'),
            (u'\u024d r with stroke', 'r-r-with-stroke'),
            (u'\u024f y with stroke', 'y-y-with-stroke'),
          ]
    ids_unicode_not_2_2 = [
           # From Latin-1 Supplements
            (u'\u00e0: a with grave', 'a-a-with-grave'),
            (u'\u00e1 a with acute', 'a-a-with-acute'),
            (u'\u00e2 a with circumflex', 'a-a-with-circumflex'),
            (u'\u00e3 a with tilde', 'a-a-with-tilde'),
            (u'\u00e4 a with diaeresis', 'a-a-with-diaeresis'),
            (u'\u00e5 a with ring above', 'a-a-with-ring-above'),
            (u'\u00e7 c with cedilla', 'c-c-with-cedilla'),
            (u'\u00e8 e with grave', 'e-e-with-grave'),
            (u'\u00e9 e with acute', 'e-e-with-acute'),
            (u'\u00ea e with circumflex', 'e-e-with-circumflex'),
            (u'\u00eb e with diaeresis', 'e-e-with-diaeresis'),
            (u'\u00ec i with grave', 'i-i-with-grave'),
            (u'\u00ed i with acute', 'i-i-with-acute'),
            (u'\u00ee i with circumflex', 'i-i-with-circumflex'),
            (u'\u00ef i with diaeresis', 'i-i-with-diaeresis'),
            (u'\u00f1 n with tilde', 'n-n-with-tilde'),
            (u'\u00f2 o with grave', 'o-o-with-grave'),
            (u'\u00f3 o with acute', 'o-o-with-acute'),
            (u'\u00f4 o with circumflex', 'o-o-with-circumflex'),
            (u'\u00f5 o with tilde', 'o-o-with-tilde'),
            (u'\u00f6 o with diaeresis', 'o-o-with-diaeresis'),
            (u'\u00f9 u with grave', 'u-u-with-grave'),
            (u'\u00fa u with acute', 'u-u-with-acute'),
            (u'\u00fb u with circumflex', 'u-u-with-circumflex'),
            (u'\u00fc u with diaeresis', 'u-u-with-diaeresis'),
            (u'\u00fd y with acute', 'y-y-with-acute'),
            (u'\u00ff y with diaeresis', 'y-y-with-diaeresis'),
           # From Latin Extended-A
            (u'\u0101 a with macron', 'a-a-with-macron'),
            (u'\u0103 a with breve', 'a-a-with-breve'),
            (u'\u0105 a with ogonek', 'a-a-with-ogonek'),
            (u'\u0107 c with acute', 'c-c-with-acute'),
            (u'\u0109 c with circumflex', 'c-c-with-circumflex'),
            (u'\u010b c with dot above', 'c-c-with-dot-above'),
            (u'\u010d c with caron', 'c-c-with-caron'),
            (u'\u010f d with caron', 'd-d-with-caron'),
            (u'\u0113 e with macron', 'e-e-with-macron'),
            (u'\u0115 e with breve', 'e-e-with-breve'),
            (u'\u0117 e with dot above', 'e-e-with-dot-above'),
            (u'\u0119 e with ogonek', 'e-e-with-ogonek'),
            (u'\u011b e with caron', 'e-e-with-caron'),
            (u'\u011d g with circumflex', 'g-g-with-circumflex'),
            (u'\u011f g with breve', 'g-g-with-breve'),
            (u'\u0121 g with dot above', 'g-g-with-dot-above'),
            (u'\u0123 g with cedilla', 'g-g-with-cedilla'),
            (u'\u0125 h with circumflex', 'h-h-with-circumflex'),
            (u'\u0129 i with tilde', 'i-i-with-tilde'),
            (u'\u012b i with macron', 'i-i-with-macron'),
            (u'\u012d i with breve', 'i-i-with-breve'),
            (u'\u012f i with ogonek', 'i-i-with-ogonek'),
            (u'\u0133 ligature ij', 'ij-ligature-ij'),
            (u'\u0135 j with circumflex', 'j-j-with-circumflex'),
            (u'\u0137 k with cedilla', 'k-k-with-cedilla'),
            (u'\u013a l with acute', 'l-l-with-acute'),
            (u'\u013c l with cedilla', 'l-l-with-cedilla'),
            (u'\u013e l with caron', 'l-l-with-caron'),
            (u'\u0140 l with middle dot', 'l-l-with-middle-dot'),
            (u'\u0144 n with acute', 'n-n-with-acute'),
            (u'\u0146 n with cedilla', 'n-n-with-cedilla'),
            (u'\u0148 n with caron', 'n-n-with-caron'),
            (u'\u014d o with macron', 'o-o-with-macron'),
            (u'\u014f o with breve', 'o-o-with-breve'),
            (u'\u0151 o with double acute', 'o-o-with-double-acute'),
            (u'\u0155 r with acute', 'r-r-with-acute'),
            (u'\u0157 r with cedilla', 'r-r-with-cedilla'),
            (u'\u0159 r with caron', 'r-r-with-caron'),
            (u'\u015b s with acute', 's-s-with-acute'),
            (u'\u015d s with circumflex', 's-s-with-circumflex'),
            (u'\u015f s with cedilla', 's-s-with-cedilla'),
            (u'\u0161 s with caron', 's-s-with-caron'),
            (u'\u0163 t with cedilla', 't-t-with-cedilla'),
            (u'\u0165 t with caron', 't-t-with-caron'),
            (u'\u0169 u with tilde', 'u-u-with-tilde'),
            (u'\u016b u with macron', 'u-u-with-macron'),
            (u'\u016d u with breve', 'u-u-with-breve'),
            (u'\u016f u with ring above', 'u-u-with-ring-above'),
            (u'\u0171 u with double acute', 'u-u-with-double-acute'),
            (u'\u0173 u with ogonek', 'u-u-with-ogonek'),
            (u'\u0175 w with circumflex', 'w-w-with-circumflex'),
            (u'\u0177 y with circumflex', 'y-y-with-circumflex'),
            (u'\u017a z with acute', 'z-z-with-acute'),
            (u'\u017c z with dot above', 'z-z-with-dot-above'),
            (u'\u017e z with caron', 'z-z-with-caron'),
           # From Latin Extended-B
            (u'\u01a1 o with horn', 'o-o-with-horn'),
            (u'\u01b0 u with horn', 'u-u-with-horn'),
            (u'\u01c6 dz with caron', 'dz-dz-with-caron'),
            (u'\u01c9 lj', 'lj-lj'),
            (u'\u01cc nj', 'nj-nj'),
            (u'\u01ce a with caron', 'a-a-with-caron'),
            (u'\u01d0 i with caron', 'i-i-with-caron'),
            (u'\u01d2 o with caron', 'o-o-with-caron'),
            (u'\u01d4 u with caron', 'u-u-with-caron'),
            (u'\u01e7 g with caron', 'g-g-with-caron'),
            (u'\u01e9 k with caron', 'k-k-with-caron'),
            (u'\u01eb o with ogonek', 'o-o-with-ogonek'),
            (u'\u01ed o with ogonek and macron', 'o-o-with-ogonek-and-macron'),
            (u'\u01f0 j with caron', 'j-j-with-caron'),
            (u'\u01f3 dz', 'dz-dz'),
            (u'\u01f5 g with acute', 'g-g-with-acute'),
            (u'\u01f9 n with grave', 'n-n-with-grave'),
            (u'\u0201 a with double grave', 'a-a-with-double-grave'),
            (u'\u0203 a with inverted breve', 'a-a-with-inverted-breve'),
            (u'\u0205 e with double grave', 'e-e-with-double-grave'),
            (u'\u0207 e with inverted breve', 'e-e-with-inverted-breve'),
            (u'\u0209 i with double grave', 'i-i-with-double-grave'),
            (u'\u020b i with inverted breve', 'i-i-with-inverted-breve'),
            (u'\u020d o with double grave', 'o-o-with-double-grave'),
            (u'\u020f o with inverted breve', 'o-o-with-inverted-breve'),
            (u'\u0211 r with double grave', 'r-r-with-double-grave'),
            (u'\u0213 r with inverted breve', 'r-r-with-inverted-breve'),
            (u'\u0215 u with double grave', 'u-u-with-double-grave'),
            (u'\u0217 u with inverted breve', 'u-u-with-inverted-breve'),
            (u'\u0219 s with comma below', 's-s-with-comma-below'),
            (u'\u021b t with comma below', 't-t-with-comma-below'),
            (u'\u021f h with caron', 'h-h-with-caron'),
            (u'\u0227 a with dot above', 'a-a-with-dot-above'),
            (u'\u0229 e with cedilla', 'e-e-with-cedilla'),
            (u'\u022f o with dot above', 'o-o-with-dot-above'),
            (u'\u0233 y with macron', 'y-y-with-macron'),
           # digraphs From Latin-1 Supplements
            (u'\u00df: ligature sz', 'sz-ligature-sz'),
            (u'\u00e6 ae', 'ae-ae'),
            (u'\u0153 ligature oe', 'oe-ligature-oe'),
            (u'\u0238 db digraph', 'db-db-digraph'),
            (u'\u0239 qp digraph', 'qp-qp-digraph'),
            ]

    def test_make_id(self):
        failures = []
        tests = self.ids + self.ids_unicode_all
        import sys
        if sys.version_info[:2] != (2, 2):
            tests += self.ids_unicode_not_2_2
        for input, expect in tests:
            output = nodes.make_id(input)
            if expect != output:
                failures.append("'%s' != '%s'" % (expect, output))
        if failures:
            self.fail("%d failures in %d\n%s" % (len(failures), len(self.ids), "\n".join(failures)))

    def test_traverse(self):
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.TextElement()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        self.assertEquals(list(e.traverse()),
                          [e, e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEquals(list(e.traverse(include_self=0)),
                          [e[0], e[0][0], e[0][1], e[0][1][0], e[1], e[2]])
        self.assertEquals(list(e.traverse(descend=0)),
                          [e])
        self.assertEquals(list(e[0].traverse(descend=0, ascend=1)),
                          [e[0], e[1], e[2]])
        self.assertEquals(list(e[0][0].traverse(descend=0, ascend=1)),
                          [e[0][0], e[0][1], e[1], e[2]])
        self.assertEquals(list(e[0][0].traverse(descend=0, siblings=1)),
                          [e[0][0], e[0][1]])
        self.testlist = e[0:2]
        self.assertEquals(list(e.traverse(condition=self.not_in_testlist)),
                          [e, e[0][0], e[0][1], e[0][1][0], e[2]])
        # Return siblings despite siblings=0 because ascend is true.
        self.assertEquals(list(e[1].traverse(ascend=1, siblings=0)),
                          [e[1], e[2]])
        self.assertEquals(list(e[0].traverse()),
                          [e[0], e[0][0], e[0][1], e[0][1][0]])
        self.testlist = [e[0][0], e[0][1]]
        self.assertEquals(list(e[0].traverse(condition=self.not_in_testlist)),
                               [e[0], e[0][1][0]])
        self.testlist.append(e[0][1][0])
        self.assertEquals(list(e[0].traverse(condition=self.not_in_testlist)),
                               [e[0]])
        self.assertEquals(list(e.traverse(nodes.TextElement)), [e[0][1]])

    def test_next_node(self):
        e = nodes.Element()
        e += nodes.Element()
        e[0] += nodes.Element()
        e[0] += nodes.TextElement()
        e[0][1] += nodes.Text('some text')
        e += nodes.Element()
        e += nodes.Element()
        self.testlist = [e[0], e[0][1], e[1]]
        compare = [(e, e[0][0]),
                   (e[0], e[0][0]),
                   (e[0][0], e[0][1][0]),
                   (e[0][1], e[0][1][0]),
                   (e[0][1][0], e[2]),
                   (e[1], e[2]),
                   (e[2], None)]
        for node, next_node in compare:
            self.assertEquals(node.next_node(self.not_in_testlist, ascend=1),
                              next_node)
        self.assertEquals(e[0][0].next_node(ascend=1), e[0][1])
        self.assertEquals(e[2].next_node(), None)

    def not_in_testlist(self, x):
        return x not in self.testlist

    def test_copy(self):
        grandchild = nodes.Text('rawsource')
        child = nodes.emphasis('rawsource', grandchild, att='child')
        e = nodes.Element('rawsource', child, att='e')
        # Shallow copy:
        e_copy = e.copy()
        self.assert_(e is not e_copy)
        # Internal attributes (like `rawsource`) are also copied.
        self.assertEquals(e.rawsource, 'rawsource')
        self.assertEquals(e_copy.rawsource, e.rawsource)
        self.assertEquals(e_copy['att'], 'e')
        # Children are not copied.
        self.assertEquals(len(e_copy), 0)
        # Deep copy:
        e_deepcopy = e.deepcopy()
        self.assertEquals(e_deepcopy.rawsource, e.rawsource)
        self.assertEquals(e_deepcopy['att'], 'e')
        # Children are copied recursively.
        self.assertEquals(e_deepcopy[0][0], grandchild)
        self.assert_(e_deepcopy[0][0] is not grandchild)
        self.assertEquals(e_deepcopy[0]['att'], 'child')


class TreeCopyVisitorTests(unittest.TestCase):

    def setUp(self):
        document = utils.new_document('test data')
        document += nodes.paragraph('', 'Paragraph 1.')
        blist = nodes.bullet_list()
        for i in range(1, 6):
            item = nodes.list_item()
            for j in range(1, 4):
                item += nodes.paragraph('', 'Item %s, paragraph %s.' % (i, j))
            blist += item
        document += blist
        self.document = document

    def compare_trees(self, one, two):
        self.assertEquals(one.__class__, two.__class__)
        self.assertNotEquals(id(one), id(two))
        self.assertEquals(len(one.children), len(two.children))
        for i in range(len(one.children)):
            self.compare_trees(one.children[i], two.children[i])

    def test_copy_whole(self):
        visitor = nodes.TreeCopyVisitor(self.document)
        self.document.walkabout(visitor)
        newtree = visitor.get_tree_copy()
        self.assertEquals(self.document.pformat(), newtree.pformat())
        self.compare_trees(self.document, newtree)


class MiscFunctionTests(unittest.TestCase):

    names = [('a', 'a'), ('A', 'a'), ('A a A', 'a a a'),
             ('A  a  A  a', 'a a a a'),
             ('  AaA\n\r\naAa\tAaA\t\t', 'aaa aaa aaa')]

    def test_normalize_name(self):
        for input, output in self.names:
            normed = nodes.fully_normalize_name(input)
            self.assertEquals(normed, output)

    def test_set_id_default(self):
        # Default prefixes.
        document = utils.new_document('test')
        # From name.
        element = nodes.Element(names=['test'])
        document.set_id(element)
        self.assertEquals(element['ids'], ['test'])
        # Auto-generated.
        element = nodes.Element()
        document.set_id(element)
        self.assertEquals(element['ids'], ['id1'])

    def test_set_id_custom(self):
        # Custom prefixes.
        document = utils.new_document('test')
        # Change settings.
        document.settings.id_prefix = 'prefix'
        document.settings.auto_id_prefix = 'auto'
        # From name.
        element = nodes.Element(names=['test'])
        document.set_id(element)
        self.assertEquals(element['ids'], ['prefixtest'])
        # Auto-generated.
        element = nodes.Element()
        document.set_id(element)
        self.assertEquals(element['ids'], ['prefixauto1'])
        

if __name__ == '__main__':
    unittest.main()
