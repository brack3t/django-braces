.. |bad| unicode:: 0x11111111

hi
-----

  indent
error

hi
-----

.. include:: <nonexistent>

.. note::

.. admonition::
   without title

.. epigraph::

.. highlights::

.. pull-quote::

.. date::

not a
definition list:
  as a term may only be one line long.

.. admonition::

   without title and content following a blank line

section underline too short
-----

==============  ======
A simple table  cell 2
==============  ======
cell 3          cell 4
==============  ======
No blank line after table.

.. |empty| unicode::

.. topic::

.. rubric::
.. rubric:: A rubric has no content

.. _`target: No matching backquote.
.. __malformed: no good

A literal block::
    with no blank line above.

::

> A literal block.
$ with inconsistent quoting.

:unknown-role:`role`
and *unbalanced
`inline
**markup

:PEP:`-1`

.. unknown:: directive (info still reported with wrong line)

==============  ======
A simple table  with
no bottom       border
