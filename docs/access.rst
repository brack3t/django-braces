Access Mixins
=============

These mixins all control a user's access to a given view. Since they all extend the ``AccessMixin``, the implement a common API that includes the following class attributes:

::

    login_url = settings.LOGIN_URL
    redirect_field_name = REDIRECT_FIELD_NAME
    raise_exception = False

The ``raise_exception`` attribute will cause the view to raise a ``PermissionDenied`` exception if it is set to ``True``, otherwise the view will redirect to the login view provided.

.. contents::

.. _LoginRequiredMixin:

LoginRequiredMixin
------------------

This mixin is rather simple and is generally the first inherited class in any of our views. If we don't have an authenticated user there's no need to go any further. If you've used Django before you are probably familiar with the ``login_required`` decorator.  All we are doing here is requiring a user to be authenticated to be able to get to this view.

While this doesn't look like much, it frees us up from having to manually overload the dispatch method on every single view that requires a user to be authenticated. If that's all that is needed on this view, we just saved 3 lines of code. Example usage below.

    .. note::
        As of version 1.0, the LoginRequiredMixin has been rewritten to behave like the rest of the ``access`` mixins. It now accepts ``login_url``, ``redirect_field_name``
        and ``raise_exception``.

    .. note::

        This should be the left-most mixin of a view, except when combined with :ref:`CsrfExemptMixin` - which in that case should be the left-most mixin.

::

    from django.views.generic import TemplateView

    from braces.views import LoginRequiredMixin


    class SomeSecretView(LoginRequiredMixin, TemplateView):
        template_name = "path/to/template.html"

        #optional
        login_url = "/signup/"
        redirect_field_name = "hollaback"
        raise_exception = True

        def get(self, request):
            return self.render_to_response({})

.. _PermissionRequiredMixin:

PermissionRequiredMixin
-----------------------

This mixin was originally written, I believe, by `Daniel Sokolowski`_ (`code here`_), but we have updated it to eliminate an unneeded render if the permissions check fails.

Rather than overloading the dispatch method manually on every view that needs to check for the existence of a permission, we inherit this class and set the ``permission_required`` class attribute on our view. If you don't specify ``permission_required`` on your view, an ``ImproperlyConfigured`` exception is raised reminding you that you haven't set it.

The one limitation of this mixin is that it can **only** accept a single permission. If you need multiple permissions use ``MultiplePermissionsRequiredMixin``.

In our normal use case for this mixin, ``LoginRequiredMixin`` comes first, then the ``PermissionRequiredMixin``. If we don't have an authenticated user, there is no sense in checking for any permissions.

    .. note::
        If you are using Django's built in auth system, ``superusers`` automatically have all permissions in your system.

::

    from braces.views import LoginRequiredMixin, PermissionRequiredMixin


    class SomeProtectedView(LoginRequiredMixin, PermissionRequiredMixin, TemplateView):
        permission_required = "auth.change_user"
        template_name = "path/to/template.html"


.. _MultiplePermissionsRequiredMixin:

MultiplePermissionsRequiredMixin
--------------------------------

The multiple permissions required view mixin is a more powerful version of the ``PermissionRequiredMixin``.  This view mixin can handle multiple permissions by setting the mandatory ``permissions`` attribute as a dict with the keys ``any`` and/or ``all`` to a list/tuple of permissions.  The ``all`` key requires the request.user to have all of the specified permissions. The ``any`` key requires the request.user to have at least ONE of the specified permissions. If you only need to check a single permission, the ``PermissionRequiredMixin`` is all you need.

    .. note::
        If you are using Django's built in auth system, ``superusers`` automatically have all permissions in your system.

::

    from braces.views import LoginRequiredMixin, MultiplePermissionsRequiredMixin


    class SomeProtectedView(LoginRequiredMixin,
                            MultiplePermissionsRequiredMixin,
                            TemplateView):

        #required
        permissions = {
            "all": ("blog.add_post", "blog.change_post"),
            "any": ("blog.delete_post", "user.change_user")
        }


.. _GroupRequiredMixin:

GroupRequiredMixin
------------------

.. versionadded:: 1.2

The group required view mixin ensures that the requesting user is in the group or groups specified. This view mixin can handle multiple groups by setting the mandatory ``group_required`` attribute as a list or tuple.

    .. note::
        The mixin assumes you're using Django's default Group model and that your user model provides ``groups`` as a ManyToMany relationship.
        If this **is not** the case, you'll need to override ``check_membership`` in the mixin to handle your custom set up.

Standard Django Usage
^^^^^^^^^^^^^^^^^^^^^

::

    from braces.views import GroupRequiredMixin


    class SomeProtectedView(GroupRequiredMixin, TemplateView):

        #required
        group_required = u'editors'


Custom Group Usage
^^^^^^^^^^^^^^^^^^

::

    from braces.views import GroupRequiredMixin


    class SomeProtectedView(GroupRequiredMixin, TemplateView):

        #required
        group_required = u'editors'

        def check_membership(self, group):
            ...
            # Check some other system for group membership
            if user_in_group:
                return True
            else:
                return False


.. _UserPassesTestMixin:

UserPassesTestMixin
------------------

.. versionadded:: dev

Mixin that reimplements the `user_passes_test` decorator. This is helpful for much more complicated cases than checking if user `is_superuser` (for example if their email is from specific a domain).

::

    from braces.views import UserPassesTestMixin

    class SomeUserPassView(UserPassesTestMixin, TemplateView):
        def test_func(self, user):
            return (user.is_staff and not user.is_superuser
                    and user.email.endswith("mydomain.com"))


.. _SuperuserRequiredMixin:

SuperuserRequiredMixin
----------------------

Another permission-based mixin. This is specifically for requiring a user to be a superuser. Comes in handy for tools that only privileged users should have access to.

::

    from braces.views import LoginRequiredMixin, SuperuserRequiredMixin


    class SomeSuperuserView(LoginRequiredMixin, SuperuserRequiredMixin, TemplateView):
        template_name = "path/to/template.html"


.. _StaffuserRequiredMixin:

StaffuserRequiredMixin
----------------------

Similar to ``SuperuserRequiredMixin``, this mixin allows you to require a user with ``is_staff`` set to True.

::

    from braces.views import LoginRequiredMixin, StaffuserRequiredMixin


    class SomeStaffuserView(LoginRequiredMixin, StaffuserRequiredMixin, TemplateView):
        template_name = "path/to/template.html"

.. _Daniel Sokolowski: https://github.com/danols
.. _code here: https://github.com/lukaszb/django-guardian/issues/48
