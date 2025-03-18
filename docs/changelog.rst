:orphan:

=========
Changelog
=========

* :release:`1.17.0 <2025-03-17>`
* :bug:`316 major` `MultiplePermissionsRequiredMixin` didn't do object-level permission checks. 
* :bug:`309 minor` `RecentLoginRequiredMixin` no longer throws error about `GET` requests in Django >= 5.0
* :release:`1.16.0 <2024-10-09>`
* :support:`290` Fixed a few typos in the docs.
* :support:`287` No longer building a universal wheel
* :support:`300` Removed unnecessarily explicit `super()` call
* :support:`301` Removed unnecessarily explicit `super()` call
* :support:`307` Use `unittest.mock` instead of `mock` package
* :feature:`305` Add support for Django 4.2
* :support:`277` Updated the codebase to be more modern Python/Django
* :feature:`245` Cache-related headers
* :release:`1.15.0 <2021-11-05>`
* :support:`-` Formatted project with black
* :support:`-` Updated README
* :feature:`265` Drop old Python and Django versions, bring in newer ones
* :release:`1.14.0 <2019-12-30>`
* :support:`260` Fixes the goshdang Travis tests.
* :support:`250` Include documentation Makefile and conf.py in source distribution.
* :support:`246` README more accurately explains the supported Django versions.
* :release:`1.13.0 <2018-04-06>`
* :support:`243` Adds support for Python 3.7 and Django 2.1.
* :support:`244` Documentation link fix.
* :support:`236` Refines the Django and Python versions tested against.
* :support:`241` Fixes a documentation typo, "altenate" should be "alternate".
* :release:`1.12.0 <2018-04-06>`
* :support:`237` Updates for Django 2.0.
* :support:`232` Updates for Django 1.11.
* :support:`227` Use SVG in README instead of PNG.
* :support:`221` Renamed a duplicative method name.
* :support:`220` Adds a warning for cases where ``prefetch_related`` or ``select_related`` are empty in their respective mixins.
* :release:`1.11.0 <2017-02-01>`
* :bug:`215 major` Imports for 1.11 and 2.x ``reverse`` and ``reverse_lazy`` functions.
* :support:`248` Include some files necessary for testing in the source distribution.
* :feature:`228` Adds an ``object_level_permissions`` attribute to the ``PermissionRequiredMixin`` to allow for object-level permission checks instead of just view-level checks.
* :bug:`224 major` Allows ``OPTIONS`` requests to be body-less.
* :bug:`218 major` ``AccessMixin.handle_no_permission` now accepts a ``request`` parameter.
* :feature:`198` New :ref:`OrderableListMixin` allows to switch the default ordering setting from `asc` to `desc`.
* :support:`215` Imports updated for Django 2.0.
* :feature:`204` New :ref:`HeaderMixin` that allows custom headers to be set on a view.
* :release:`1.10.0 <2016-10-24>`
* :bug:`212 major` Small changes for Django 1.10 compatibility.
* :bug:`211 major` ReadTheDocs links updated.
* :bug:`209 major` Django documentation link updated.
* :release:`1.9.0 <2016-05-31>`
* :bug:`208 major` Fixed errors from combining certain access mixins.
* :bug:`196 major` Refactor how users without permissions are handled.
* :bug:`181 major` Fixed redirect loops based on user permissions.
* :bug:`161 major` Fixed redirect loop for users without proper groups for ``MultipleGroupRequiredMixin`` and ``GroupRequiredMixin``.
* :support:`209` Fixed link to Django documentation for ``user_passes_test`` decorator.
* :feature:`203` Use Django's supplied version of ``six`` to remove an external dependency.
* :support:`202` Fixed typo in ``PermissionsRequiredMixin`` and ``MultiplePermissionsRequiredMixin``.
* :support:`201` Fixed typo in ``SuccessURLRedirectListMixin``.
* :support:`192` Added example for ``OrderableListView``.
* :release:`1.8.1 <2015-07-12>`
* :bug:`176` Only check time delta for authenticated users in :ref:`RecentLoginRequiredMixin`.
* :bug:`-` Changed :ref:`JsonRequestResponseMixin` docs to not use `ugettext_lazy`.
* :bug:`-` Updated tests to include Python 3.2.
* :bug:`185` Removed `u` prefixes to allow Python 3.2 support.
* :support:`-` Added note to docs about Python and Django versions used in tests.
* :bug:`-` Fix small issue in docs for :ref:JsonResponseMixin. The accepted kwarg for the render_to_response method is status not status_code.
* :release:`1.8.0 <2015-04-16>`
* :support:`145` Allow custom exceptions to be raised by all AccessMixins.
* :feature:`171` New ``SSLRequiredMixin``. Redirect http -> https.
* :feature:`138` New :ref:`RecentLoginRequiredMixin` to require user sessions to have a given freshness.
* :bug:`164 major` Use `resolve_url` to handle `LOGIN_REDIRECT_URL`s in `settings.py` that are just URL names.
* :bug:`130 major` New attribute on :ref:`JSONResponseMixin` to allow setting a custom JSON encoder class.
* :bug:`131 major` New attribute on :ref:`LoginRequiredMixin` so it's possible to redirect unauthenticated users while
  using ``AccessMixin``-derived mixins instead of throwing an exception.
* :release:`1.4.0 <2014-03-04>`
* :support:`129` Split ``views.py`` out into multiple files since it was approaching 1000 LoC.
* :feature:`119` :ref:`SetHeadlineMixin` now accepts ``headline`` with ``ugettext_lazy()``-wrapped strings.
* :bug:`94 major` Fixed a bug where :ref:`JSONResponseMixin` would override the ``content_type`` of Django's ``TemplateView`` in Django 1.6.
* :bug:`- major` Fixed bug in :ref:`PermissionRequiredMixin` where if ``PermissionRequiredMixin.no_permissions_fail`` returned a false-y value, the user lacking the permission would pass instead of being denied access.
* :support:`73` Added doc for how to contribute.
* :feature:`120` Added :ref:`MessageMixin` to allow easier access to Django's ``contrib.messages`` messages. :ref:`FormValidMessageMixin` and :ref:`FormInvalidMessageMixin` were updated to use it.
* :bug:`98 major` Fixed bug in :ref:`CanonicalSlugDetailMixin` to allow it to use custom URL kwargs.
* :bug:`105 major` Fixed bug in :ref:`GroupRequiredMixin` where superusers were blocked by lack of group memberships.
* :bug:`106 major` Fixed bug in :ref:`GroupRequiredMixin` which now correctly checks for group membership against a list.
* :feature:`102` Added new :ref:`StaticContextMixin` mixin which lets you pass in ``static_context`` as a property of the view.
* :feature:`89` Added new :ref:`AnonymousRequiredMixin` which redirects authenticated users to another view.
* :feature:`104` Added new :ref:`AllVerbsMixin` which allows a single method to response to all HTTP verbs.
* :bug:`- major` Provided ``JSONRequestResponseMixin`` as a mirror of :ref:`JsonRequestResponseMixin` because we're not PHP.
* :feature:`107` :ref:`FormValidMessageMixin`, :ref:`FormInvalidMessageMixin`, and :ref:`FormMessagesMixin` all allow ``ugettext_lazy``-wrapped strings.
* :feature:`67` Extended :ref:`PermissionRequiredMixin` and :ref:`MultiplePermissionsRequiredMixin` to accept django-guardian-style custom/object permissions.
* :release:`1.3.1 <2014-01-04>`
* :bug:`95` Removed accidentally-added breakpoint.
* :support:`96 backported` Added ``build/`` to ``.gitignore``
* :release:`1.3.0 <2014-01-03>`
* :support:`59` Removed ``CreateAndRedirectToEditView`` mixin which was marked for deprecation and removal since 1.0.0.
* :feature:`51` Added :ref:`JsonRequestResponseMixin` which attempts to parse requests as JSON.
* :feature:`61` Added :ref:`CanonicalSlugDetailMixin` mixin which allows for the specification of a canonical slug on a ``DetailView`` to help with SEO by redirecting on non-canonical requests.
* :feature:`76` Added :ref:`UserPassesTestMixin` mixin to replicate the behavior of Django's ``@user_passes_test`` decorator.
* :bug:`- major` Some fixes for :ref:`CanonicalSlugDetailMixin`.
* :feature:`92` ``AccessMixin`` now has a runtime-overridable ``login_url`` attribute.
* :bug:`- major` Fixed problem with :ref:`GroupRequiredMixin` that made it not actually work.
* :support:`-` All tests pass for Django versions 1.4 through 1.6 and Python versions 2.6, 2.7, and 3.3 (Django 1.4 and 1.5 not tested with Python 3.3).
* :release:`1.2.2 <2013-08-07>`
* :support:`-` Uses ``six.string_types`` instead of explicitly relying on ``str`` and ``unicode`` types.
* :release:`1.2.1 <2013-07-28>`
* :bug:`-` Fix to allow ``reverse_lazy`` to work for all ``AccessMixin``-derived mixins.
* :release:`1.2.0 <2013-07-27>`
* :feature:`57` :ref:`FormValidMessageMixin` which provides a ``messages`` message when the processed form is valid.
* :feature:`-` :ref:`FormInvalidMessageMixin` which provides a ``messages`` message when the processed form is invalid.
* :feature:`-` :ref:`FormMessagesMixin` which provides the functionality of both of the above mixins.
* :feature:`-` :ref:`GroupRequiredMixin` which is a new access-level mixin which requires that a user be part of a specified group to access a view.
* :release:`1.1.0 <2013-07-18>`
* :bug:`52 major` :ref:`JSONResponseMixin` ``.render_json_response`` method updated to accept a status code.
* :bug:`43 major` :ref:`JSONResponseMixin` added ``json_dumps_kwargs`` attribute & get method to pass args to the JSON encoder.
* :feature:`45` New :ref:`OrderableListMixin` allows ordering of list views by GET params.
* :support:`-` Tests updated to test against latest stable Django release (1.5.1)
* :support:`-` Small fixes and additions to documentation.
* :release:`1.0.0 <2013-02-28>`
* :feature:`-` New 'abstract' ``AccessMixin`` which provides overridable ``get_login_url`` and ``get_redirect_field_name`` methods for all access-based mixins.
* :feature:`32` Rewritten :ref:`LoginRequiredMixin` which provides same customization as other access mixins with ``login_url``, ``raise_exception`` & ``redirect_field_name``.
* :feature:`33` New :ref:`PrefetchRelatedMixin`. Works the same as :ref:`SelectRelatedMixin` but uses Django's ``prefetch_related`` method.
* :support:`-` ``CreateAndRedirectToEditView`` is marked for deprecation.
* :bug:`- major` :ref:`PermissionRequiredMixin` no longer requires dot syntax for permission names.
* :support:`-` Marked package as supporting 2.6 thru 3.3 (from rafales).
* :support:`-` Fixes to documentation.
* :support:`-` Tests to cover new additions and changes.
* :release:`0.2.3 <2013-02-22>`
* :support:`30 backported` Tests for all mixins (from rafales).
* :feature:`26 backported` New :ref:`CsrfExemptMixin` for marking views as being CSRF exempt (from jarcoal).
* :support:`- backported` Some documentation updates and a spelling error correction (from shabda).
* :bug:`-` :ref:`SuccessURLRedirectListMixin` raises ``ImproperlyConfigured`` if no ``success_list_url`` attribute is supplied (from kennethlove).
* :release:`0.2.2 <2013-01-21>`
* :bug:`25` Try importing the built-in ``json`` module first, drop back to Django if necessary.
* :support:`- backported` Django 1.5 compatibility.
* :release:`0.2.1 <2012-12-10>`
* :bug:`21 major` Fixed signature of :ref:`UserFormKwargsMixin` ``.get_form_kwargs``
* :feature:`22` Updated :ref:`JSONResponseMixin` to work with non-ASCII characters and other datatypes (such as datetimes)
* :bug:`- major` Fixed all mixins that have ``raise_exception`` as an argument to properly raise a ``PermissionDenied`` exception to allow for custom 403s.
