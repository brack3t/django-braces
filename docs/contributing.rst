:orphan:

============
Contributing
============

First of all, thank you for wanting to make **django-braces** better! We love
getting input and suggestions from the community.

Secondly, we just want to put out a few ground rules for contributing so that
we can get your pull requests in sooner and cause less headaches all around.

.. _Code Style:

Code Style
----------

We stick to `PEP8 <http://legacy.python.org/dev/peps/pep-0008/>`_ as much as
possible, so please make sure your code passes a lint test. That means two
blank lines before class names and all of those other wonderful rules.

We like docstrings in the classes, too. This helps us and those that come
later what the class is meant to do. Docstrings in methods are great, too,
especially if the method makes any assumptions about how it'll be used.


.. _Docs:

Docs
----

If you're reading this, you should already know that docs are important to
this project and, honestly, all of them. We like any new mixins, or changes
in existing mixins, to come with documentation changes showing how to use
the mixin. Ideally, you show at least one example usage, but if your mixin
provides multiple paths, perhaps a static attribute or a dynamic method,
it's really great if your documentation shows both avenues, too.

.. _Tests:

Tests
-----

All code changes should come with test changes. We use
`py.test <https://pypi.python.org/pypi/pytest>`_ instead of Python's
``unittest``. This seems to only be really important when marking tests for
skipping. 

We try to keep the project at 100% test coverage but know this isn't something
we can achieve forever. So long as your tests cover your contribution 80% or
better, we're happy to try and bump up that last bit, or just accept the code.

We currently test Braces against late (usually latest) versions of Python 2.7, 3.2, 3.3, 3.4 and 3.5. We also test against the latest released version of Django from 1.5 to 1.10.
