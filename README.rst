Here are the generic mixins we've been using a lot lately. If you have any that you find useful, feel free to send them to us in a pull request. Please include example usage.

http://django-braces.readthedocs.org/en/latest/index.html

CONTRIBUTING
============

Fork, make a change, update the docs, make a pull request.

Add yourself to CONTRIBUTORS.txt if you want.

CHANGE LOG
==========

1.2.1
-----

* Fixed signature of UserFormKwargsMixin.get_form_kwargs
* Updated JSONResponseMixin to work with non-ASCII characters and other datatypes (such as
  datetimes)
* Fixed all mixins that have `raise_exception` as an argument to properly raise a
  `PermissionDenied` exception to allow for custom 403s.

1.2.2
-----

* Try importing the built-in json module first, drop back to Django if necessary. Django 1.5 compatibility.
