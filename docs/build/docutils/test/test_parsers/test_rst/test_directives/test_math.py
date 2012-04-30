#! /usr/bin/env python

# $Id: test_math.py 7062 2011-06-30 22:14:29Z milde $
# Author: Guenter Milde <milde@users.sf.net>
# Copyright: This module has been placed in the public domain.

"""
Tests for the 'math' directive.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['argument'] = [
["""\
.. math:: y = f(x)
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        y = f(x)
"""],
]

totest['content'] = [
["""\
.. math::

  1+1=2
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        1+1=2
"""],
]

totest['options'] = [
["""\
.. math::
  :class: new
  :name: eq:Eulers law

  e^i*2*\pi = 1
""",
"""\
<document source="test data">
    <math_block classes="new" ids="eq-eulers-law" names="eq:eulers\ law" xml:space="preserve">
        e^i*2*\pi = 1
"""],
]

totest['argument_and_content'] = [
["""\
.. math:: y = f(x)

  1+1=2

""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        y = f(x)
    <math_block xml:space="preserve">
        1+1=2
"""],
]

totest['content with blank line'] = [
["""\
.. math::

  1+1=2
  
  E = mc^2
""",
"""\
<document source="test data">
    <math_block xml:space="preserve">
        1+1=2
    <math_block xml:space="preserve">
        E = mc^2
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
