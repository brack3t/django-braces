Test the interaction of transforms.references.Substitutions and
transforms.references.ExternalLinks.

|rest| is cool!

.. |rest| replace:: reStructuredText_

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

There is a preferred alternative:

|rst|_ is cool!

.. |rst| replace:: reStructuredText
.. _rst: http://docutils.sourceforge.net/rst.html

This only works for the case where the entire substitution is a
reference, not when the reference is embedded as part of the
substitution:

|urst| is uncool!

.. |urst| replace:: unstructured reStructuredText_
