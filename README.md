# django-braces

Mixins for Django's class-based views.

[![Latest Travis CI status](https://travis-ci.org/brack3t/django-braces.svg)](https://travis-ci.org/brack3t/django-braces)
[![PyPI version](https://badge.fury.io/py/django-braces.svg)](http://badge.fury.io/py/django-braces)

## Documentation
[Read The Docs](https://django-braces.readthedocs.io/en/latest/index.html)

## Installation
Install from PyPI with `pip`:
`pip install django-braces`

## Building the Docs
1. Install docs requirements: `pip install -r requirements-docs.txt`.
2. `cd docs`.
3. `make html`.
4. Open `_build/index.html` in your browser.

## Contributing

See our [contribution guide](https://django-braces.readthedocs.io/en/latest/contributing.html)

Add yourself to `CONTRIBUTORS.txt` if you want.

All development dependencies are available in `requirements.txt` file.

To run the test suite, please install `tox` and as many Python interpreters as you'd
like to test against. Currently we test against 2.7, 3.6, 3.7, and 3.8. We recommend
using `asdf` to install various Python versions.

Once `tox` and Python(s) are installed, you can execute the entire suite by running
just the `tox` command.


## Change Log

[Changelog on Read The Docs](https://django-braces.readthedocs.io/en/latest/changelog.html)

## Supported Django Versions

Our policy is that `django-braces` officially supports, and is tested on, all versions
that Django [officially supports](https://www.djangoproject.com/download/#supported-versions).
You are free to use `django-braces` with any version of Django you wish (so long as it has
class-based views) but no support will be promised.
