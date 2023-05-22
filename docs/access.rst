Access Mixins
=============

These mixins control access to a view in various ways.

.. contents::

.. _PassesTestMixin:

PassesTestMixin
---------------

.. versionadded:: 2.0

The ``PassesTestMixin`` is one of the simplest mixins in ``django-braces`` and
is the parent class for most of the access mixins. ``PassesTest`` comes with
one attribute ``dispatch_test`` and one method ``handle_test_failure`` that
should be customized.

``dispatch_test`` should be a string which is the name of a method on the class
that has to pass (return ``True``) for the view to be dispatched. By default,
this will raise ``PermissionDenied`` if the test fails. This can be overridden
by providing your own implementation of ``handle_test_failure``.

::

    from django.views.generic import View

    from braces.mixins import PassesTestMixin

    class UsernameRequiredView(PassesTestMixin, View):
        dispatch_test = "username_is_not_bob"

        def username_is_not_bob(self):
            return self.request.user.username != "bob"

        def handle_test_failure(self):
            return PermissionDenied("Get out of here, Bob!")


.. _SuperuserRequiredMixin:

SuperuserRequiredMixin
----------------------

The ``SuperuserRequiredMixin`` checks that the requesting user is a superuser.
If they are not, the usual ``PermissionDenied`` exception will be raised.

The default test is named ``test_superuser``. Override this method if you need
to determine superuser status in a different way.

::

    from braces.mixins import SuperuserRequiredMixin


    class SuperuserView(SuperuserRequiredMixin, View):
        """Only superusers can delete widgets."""
        ...


.. _StaffuserRequiredMixin:

StaffuserRequiredMixin
----------------------

The ``StaffuserRequiredMixin`` is similar to the :ref:`SuperuserRequiredMixin`
but instead of checking for ``is_superuser``, it checks for ``is_staff``.

::

    from braces.mixins import StaffuserRequiredMixin


    class SomeStaffuserView(views.StaffuserRequiredMixin, View):
        """Staff users are required for changing grades."""
        ...


.. _GroupRequiredMixin:

GroupRequiredMixin
------------------

Instead of requiring a user attribute like :ref:`SuperuserRequiredMixin` or
:ref:`StaffUserRequiredMixin`, the ``GroupRequiredMixin`` ensures that the
requesting user is in the group or groups specified. ``group_required``
should be a string or list of strings representing the name of the
group(s) required.

.. note::
    The mixin assumes you're using Django's default Group model and
    that your user model provides ``groups`` as a ManyToMany
    relationship. If this **is not** the case, you'll need to
    override ``check_membership`` to handle your custom set up.

::

    from braces.mixins import GroupRequiredMixin


    class IlluminatiOnlyView(GroupRequiredMixin, View):
        """Keep the plebs out of our plans."""

        group_required = "illuminati"
        # OR #
        group_required = ["illuminati", "freemasons"]


.. _AnonymousRequiredMixin:

AnonymousRequiredMixin
----------------------

The `AnonymousRequiredMixin` will reject all requests from authenticated users.
Override the `test_anonymous` method to customize anonymous user detection.


::

    from braces.mixins import AnonymousRequiredMixin


    class AnonOnly(AnonymousRequiredMixin, View):
        """Anonymous users only."""
        ...



.. _LoginRequiredMixin:

LoginRequiredMixin
------------------

Use this mixin when you want to reject unauthenticated requests.

::

    from braces.mixins import LoginRequiredMixin


    class SomeSecretView(LoginRequiredMixin, View):
        """Secrets for authenticated users only."""

By default, a failing request causes ``PermissionDenied`` to be raised. You can
customize this by overriding ``handle_test_failure`` like other :ref:`PassesTestMixin`
descendants.


.. _RecentLoginRequiredMixin:

RecentLoginRequiredMixin
------------------------

Views using this mixin will reject requests without recent authentication.
The default timeout is 1800 seconds, or 30 minutes. You can change this
by providing a new value, in seconds, for ``max_age``.

::

    from braces.mixins import RecentLoginRequiredMixin


    class NewLoginsOnly(RecentLoginRequiredMixin, View):
        max_last_login_delta = 600  # Require a login within the last 10 minutes
        ...


.. _PermissionRequiredMixin:

PermissionRequiredMixin
-----------------------

.. note::
    This mixin was originally written by `Daniel Sokolowski`_ (`code here`_). The mixin has changed since then.

.. note::
    If you are using Django's built in auth system, ``superusers`` automatically have all permissions.

Use the ``PermissionRequiredMixin`` when you need to ensure that a request
is from a user with particular permissions. The mixin can handle both
required and optional permissions simultaneously.

Your ``permission_required`` attribute should be a dictionary with two keys,
``all`` and ``any``. The values for each key should be a list of strings
representing the required/optional permissions.

::

    from braces.mixins import PermissionRequiredMixin


    class SomeProtectedView(PermissionRequiredMixin, View):
        permission_required = {
            "all": ["blog.add_post", "blog.change_post"],
            "any": ["secrets.can_view"]
        }

The above view would require a requesting user to:

* Have the ``blog.add_post`` and ``blog.change_post`` permissions **and**
* Optionally have the ``secrets.can_view`` permission


.. _SSLRequiredMixin:

SSLRequiredMixin
----------------

This mixin will reject or redirect all non-SSL requests. By default, the
mixin will attempt a redirect to the SSL version of the requested URL.
If you'd prefer to raise ``BadRequest``, set ``redirect_to_ssl`` to ``False``.

::

    from braces.mixins import SSLRequiredMixin


    class SecureConnectionRequired(SSLRequiredMixin, View):
        """Redirects from http -> https."""
        ...


.. _Daniel Sokolowski: https://github.com/danols
.. _code here: https://github.com/lukaszb/django-guardian/issues/48
