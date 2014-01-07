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

## Contributing

Fork, make a change, update the docs, add/update tests, make a pull request.

Add yourself to `CONTRIBUTORS.txt` if you want.

All development dependencies are available in `requirements.txt` file.

To run the test suite, execute the following in your shell (Django install is required):
`py.test tests/ --cov=braces --cov-report=html`

## Change Log

### 1.3.1

* Removed breakpoint that was added accidentally.
* Added the build directory to `.gitignore`

### 1.3.0

* Removed `CreateAndRedirectToEditView` mixin. It was marked for deprecation and removal since 1.0.
* Added `JsonRequestAndResponseMixin` mixin which attempts to parse requests as JSON.
* Added `CanonicalSlugDetailMixin` mixin which allows for the specification of a canonical slug on a `DetailView` to help with SEO by redirecting on non-canonical requests.
* Added `UserPassesTestMixin` mixin to replicate the behavior of Django's `@user_passes_test` decorator.
* Some fixes for `CanonicalSlugDetailMixin`.
* `AccessMixin` now has a runtime-overridable `login_url` attribute.
* Fixed problem with `GroupRequiredMixin` that made it not actually work.
* All tests pass for Django versions 1.4 through 1.6 and Python versions 2.6, 2.7, and 3.3 (Django 1.4 and 1.5 not tested with Python 3.3).
* Tests and documentation changes for all of the above.

### 1.2.1
* Fix to allow `reverse_lazy` on all `AccessMixin`-derived mixins.

### 1.2.0
* `FormValidMessageMixin` which provides a `messages` message when the processed form is valid.
* `FormInvalidMessageMixin` which provides a `messages` message when the processed form is invalid.
* `FormMessagesMixin` which provides the functionality of both of the above mixins.
* `GroupRequiredMixin` which is a new access-level mixin which requires that a user be part of a specified group to access a view.

### 1.1.0
* `JSONResponseMixin.render_json_response` method updated to accept a status code.
* `JSONResponseMixin` added `json_dumps_kwargs` attribute & get method to pass args to the json encoder.
* New `OrderableListMixin` allows ordering of list views by GET params.
* Tests updated to test against latest stable Django release (1.5.1)
* Small fixes and additions to documentation.

### 1.0.0
* New 'abstract' `AccessMixin` which provides overridable `get_login_url` and `get_redirect_field_name methods` for all access-based mixins.
* Rewritten `LoginRequiredMixin` which provides same customization as other access mixins with `login_url`, `raise_exception` & `redirect_field_name`.
* New `PrefetchRelatedMixin`. Works the same as `SelectRelatedMixin` but uses Django's `prefetch_related` method.
* `CreateAndRedirectToEditView` is marked for deprecation.
* `PermissionRequiredMixin` no longer requires dot syntax for permission names.
* Marked package as supporting 2.6 thru 3.3 (from rafales).
* Fixes to documentation.
* Tests to cover new additions and changes.

### 0.2.3
* Tests for all mixins (from rafales).
* New `CsrfExemptMixin` for marking views as being CSRF exempt (from jarcoal).
* Some documentation updates and a spelling error correction (from shabda).
* `SuccessURLRedirectListMixin` raises `ImproperlyConfigured` if no `success_list_url` attribute is supplied (from kennethlove).

### 0.2.2
* Try importing the built-in json module first, drop back to Django if necessary. Django 1.5 compatibility.

### 0.2.1
* Fixed signature of `UserFormKwargsMixin.get_form_kwargs`
* Updated `JSONResponseMixin` to work with non-ASCII characters and other datatypes (such as datetimes)
* Fixed all mixins that have `raise_exception` as an argument to properly raise a `PermissionDenied` exception to allow for custom 403s.
