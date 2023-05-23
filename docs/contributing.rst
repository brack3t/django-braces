:orphan:

============
Contributing
============

First of all, thank you for wanting to make **django-braces** better! We love
getting input and suggestions from the community.

Secondly, we just want to put out a few ground rules for contributing so that
we can get your pull requests in sooner and cause fewer headaches all around.

.. _Code Style:

Code Style
----------

We use ``black`` to format our code. Please run ``black`` on your code
before submitting it. We also use ``interrogate`` to check for docstrings.
We also use ``mypy`` for type checking. Please run ``mypy`` on your code.

.. _Tests:

Tests
-----

All code changes should come with test changes. We use ``pytest``
instead of Python's ``unittest``. We try to keep the project at a high
test coverage but know this isn't a magic bullet. Tests should be
included with your pull requests and should cover as close to 100% of
your changes as possible.

We test ``django-braces`` against currently supported versions of Python
and LTS versions of Django. All pull requests are run through a matrix
of these Python and Django versions. During development, we use a recent
Python and Django.

We're big fans of ``pytest``'s fixtures, so please feel free to provide
fixtures for your tests.

.. _Docs:

Docs
----

If you're reading this, you should already know that docs are important to
this project and, honestly, all of them. We like any new mixins, or changes
in existing mixins, to come with documentation changes showing how to use
the mixin. Ideally, you show at least one example usage.
