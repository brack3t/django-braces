:orphan:

=========
Changelog
=========

* :bug:`130` New attribute on :ref:`JSONResponseMixin` to allow setting a custom JSON encoder class.
* :bug:`131` New attribute on :ref:`LoginRequiredMixin` so it's possible to redirect unauthenticated users while
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
* :support:`30` Tests for all mixins (from rafales).
* :feature:`26` New :ref:`CsrfExemptMixin` for marking views as being CSRF exempt (from jarcoal).
* :support:`-` Some documentation updates and a spelling error correction (from shabda).
* :bug:`-` :ref:`SuccessURLRedirectListMixin` raises ``ImproperlyConfigured`` if no ``success_list_url`` attribute is supplied (from kennethlove).
* :release:`0.2.2 <2013-01-21>`
* :bug:`25` Try importing the built-in ``json`` module first, drop back to Django if necessary.
* :support:`-` Django 1.5 compatibility.
* :release:`0.2.1 <2012-12-10>`
* :bug:`21 major` Fixed signature of :ref:`UserFormKwargsMixin` ``.get_form_kwargs``
* :feature:`22` Updated :ref:`JSONResponseMixin` to work with non-ASCII characters and other datatypes (such as datetimes)
* :bug:`- major` Fixed all mixins that have ``raise_exception`` as an argument to properly raise a ``PermissionDenied`` exception to allow for custom 403s.
