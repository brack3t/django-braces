.. image:: https://drone.io/github.com/brack3t/django-braces/status.png
    :target: https://drone.io/github.com/brack3t/django-braces
    :alt: Latest drone.io status

.. image:: https://pypip.in/v/django-braces/badge.png
    :target: https://crate.io/packages/django-braces/
    :alt: Latest PyPI version

.. image:: https://pypip.in/d/django-braces/badge.png
    :target: https://crate.io/packages/django-braces/
    :alt: Number of PyPI downloads

Here are the generic mixins we've been using a lot lately. If you have any that you find useful, feel free to send them to us in a pull request. Please include example usage.

http://django-braces.readthedocs.org/en/latest/index.html

CONTRIBUTING
============

Fork, make a change, update the docs, make a pull request.

Add yourself to CONTRIBUTORS.txt if you want.

All development dependencies are available in requirements.txt file.

To run the test suite, execute the following in your shell (Django install is required):

    `py.test tests/ --cov=braces --cov-report=html`


CHANGE LOG
==========

1.1.0
-----
* ``JSONResponseMixin.render_json_response`` method updated to accept a status code.
* ``JSONResponseMixin`` added ``json_dumps_kwargs`` attribute & get method to pass args to the json encoder.
* New ``OrderableListMixin`` allows ordering of list views by GET params.
* Tests updated to test against latest stable Django release (1.5.1)
* Small fixes and additions to documentation.

1.0.0
-----
* New 'abstract' ``AccessMixin`` which provides overridable ``get_login_url`` and ``get_redirect_field_name methods`` for all access-based mixins.
* Rewritten ``LoginRequiredMixin`` which provides same customization as other access mixins with ``login_url``, ``raise_exception`` & ``redirect_field_name``.
* New ``PrefetchRelatedMixin``. Works the same as ``SelectRelatedMixin`` but uses Django's ``prefetch_related`` method.
* ``CreateAndRedirectToEditView`` is marked for deprecation.
* ``PermissionRequiredMixin`` no longer requires dot syntax for permission names.
* Marked package as supporting 2.6 thru 3.3 (from rafales).
* Fixes to documentation.
* Tests to cover new additions and changes.

0.2.3
-----

* Tests for all mixins (from rafales).
* New ``CsrfExemptMixin`` for marking views as being CSRF exempt (from jarcoal).
* Some documentation updates and a spelling error correction (from shabda).
* ``SuccessURLRedirectListMixin`` raises ``ImproperlyConfigured`` if no ``success_list_url`` attribute is supplied (from kennethlove).

0.2.2
-----

* Try importing the built-in json module first, drop back to Django if necessary. Django 1.5 compatibility.

0.2.1
-----

* Fixed signature of ``UserFormKwargsMixin.get_form_kwargs``
* Updated ``JSONResponseMixin`` to work with non-ASCII characters and other datatypes (such as
  datetimes)
* Fixed all mixins that have ``raise_exception`` as an argument to properly raise a
  ``PermissionDenied`` exception to allow for custom 403s.

