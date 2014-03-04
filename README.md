# django-braces
Mixins for Django's class-based views.

[![Latest drone.io status](https://drone.io/github.com/brack3t/django-braces/status.png)](https://drone.io/github.com/brack3t/django-braces)
[![Latest PyPI version](https://pypip.in/v/django-braces/badge.png)](https://crate.io/packages/django-braces/)
[![Number of PyPI downloads](https://pypip.in/d/django-braces/badge.png)](https://crate.io/packages/django-braces/)
[![Stories in Ready](https://badge.waffle.io/brack3t/django-braces.png)](http://waffle.io/brack3t/django-braces)

## Documentation
[Read The Docs](http://django-braces.readthedocs.org/en/latest/index.html)

## Installation
Install from PyPI with `pip`:
`pip install django-braces`

## Building the Docs
1. Install docs requirements: `pip install -r requirements-docs.txt`.
2. `cd docs`.
3. `make html`.
4. Open `_build/index.html` in your browser.

## Contributing

Fork, make a change, update the docs, add/update tests, make a pull request.

Add yourself to `CONTRIBUTORS.txt` if you want.

All development dependencies are available in `requirements.txt` file.

To run the test suite, execute the following in your shell (Django install is required):
`py.test tests/ --cov=braces --cov-report=html`

## Change Log

[Changelog on Read The Docs](https://django-braces.readthedocs.org/en/latest/changelog.html)
