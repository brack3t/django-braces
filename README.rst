Here are the generic mixins we've been using a lot lately. If you have any that you find useful, feel free to send them to us in a pull request. Please include example usage.

http://django-braces.readthedocs.org/en/latest/index.html

CONTRIBUTING
============

Fork, make a change, update the docs, make a pull request.

Add yourself to CONTRIBUTORS.txt if you want.

All development dependencies are available in requirements.txt file.

To run test suite execute this in your shell:

    `py.test --cov=braces --cov-report=html`


CHANGE LOG
==========

0.2.3
-----

* Tests for all mixins (from rafales).
* New CsrfExemptMixin for marking views as being CSRF exempt (from jarcoal).
* Some documentation updates and a spelling error correction (from shabda).
* SuccessURLRedirectListMixin raises ImproperlyConfigured if no success_list_url attribute is supplied (from kennethlove).

0.2.2
-----

* Try importing the built-in json module first, drop back to Django if necessary. Django 1.5 compatibility.

0.2.1
-----

* Fixed signature of UserFormKwargsMixin.get_form_kwargs
* Updated JSONResponseMixin to work with non-ASCII characters and other datatypes (such as
  datetimes)
* Fixed all mixins that have `raise_exception` as an argument to properly raise a
  `PermissionDenied` exception to allow for custom 403s.

