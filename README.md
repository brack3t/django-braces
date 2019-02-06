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

To run the test suite, execute the following in your shell (Django install is required):
`py.test tests/ --cov=braces --cov-report=html`

Or test with `tox` if you have `tox` installed.

## Change Log

[Changelog on Read The Docs](https://django-braces.readthedocs.io/en/latest/changelog.html)

## Supported Django Versions

Our policy is that `django-braces` officially supports, and is tested on, Django 2.0 and Django 1.11 LTS. There won't be any restraints on using other versions of Django, though, but it will be a "buyer beware" situation.
