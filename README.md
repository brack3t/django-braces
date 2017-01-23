# django-braces
Mixins for Django's class-based views.

[![Latest Travis CI status](https://travis-ci.org/brack3t/django-braces.svg)](https://travis-ci.org/brack3t/django-braces)
[![PyPI version](https://badge.fury.io/py/django-braces.png)](http://badge.fury.io/py/django-braces)

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

## Use Django 1.4.x?

`django-braces` 1.4.x will be the last version to officially support Django 1.4.x. Since Django 1.4.x is an LTS, we'll update `django-braces` 1.4.x as needed for bug fixes but it won't receive new functionality unless backporting is 100% painless.

Our policy going forward is that `django-braces` officially supports the current version of Django and one version each direction (e.g. 1.6.x is current, so 1.5.x, 1.6.x, and 1.7.x are all supported). There won't be any restraints on using other versions of Django, though, but it will be a "buyer beware" situation.
