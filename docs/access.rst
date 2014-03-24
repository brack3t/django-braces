Access Mixins
=============

These mixins all control a user's access to a given view. Since many of them extend the ``AccessMixin``, the following are common attributes:

::

    login_url = settings.LOGIN_URL
    redirect_field_name = REDIRECT_FIELD_NAME
    raise_exception = False

The ``raise_exception`` attribute will cause the view to raise a ``PermissionDenied`` exception if it is set to ``True``, otherwise the view will redirect to the login view provided.

.. contents::

.. _LoginRequiredMixin:

LoginRequiredMixin
------------------

This mixin is rather simple and is generally the first inherited class in any view. If you don't have an authenticated user, there's no need to go any further. If you've used Django before you are probably familiar with the ``login_required`` decorator.  This mixin replicates the decorator's functionality.

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

An optional class attribute of ``redirect_unauthenticated_users`` can be set to ``True`` if you are using another ``access`` mixin with ``raise_exception`` set to ``True``. This will redirect to the login page if the user is not authenticated, but raises an exception if they are but do not have the required access defined by the other mixins. This defaults to ``False``.

.. _PermissionRequiredMixin:

PermissionRequiredMixin
-----------------------

This mixin was originally written by `Daniel Sokolowski`_ (`code here`_), but this version eliminates an unneeded render if the permissions check fails.

Rather than overloading the dispatch method manually on every view that needs to check for the existence of a permission, use this mixin and set the ``permission_required`` class attribute on your view. If you don't specify ``permission_required`` on your view, an ``ImproperlyConfigured`` exception is raised reminding you that you haven't set it.

The one limitation of this mixin is that it can **only** accept a single permission. If you need multiple permissions use :ref:`MultiplePermissionsRequiredMixin`.

In normal use of this mixin, :ref:`LoginRequiredMixin` comes first, then the ``PermissionRequiredMixin``. If the user isn't an authenticated user, there is no point in checking for any permissions.

    .. note::
        If you are using Django's built in auth system, ``superusers`` automatically have all permissions in your system.

::

    from django.views import TemplateView

    from braces import views


    class SomeProtectedView(views.LoginRequiredMixin,
                            views.PermissionRequiredMixin,
                            TemplateView):

        permission_required = "auth.change_user"
        template_name = "path/to/template.html"

The ``PermissionRequiredMixin`` also offers a ``check_permssions`` method that should be overridden if you need custom permissions checking.


.. _MultiplePermissionsRequiredMixin:

MultiplePermissionsRequiredMixin
--------------------------------

The ``MultiplePermissionsRequiredMixin`` is a more powerful version of the :ref:`PermissionRequiredMixin`.  This view mixin can handle multiple permissions by setting the mandatory ``permissions`` attribute as a dict with the keys ``any`` and/or ``all`` to a list or tuple of permissions.  The ``all`` key requires the ``request.user`` to have **all** of the specified permissions. The ``any`` key requires the ``request.user`` to have **at least one** of the specified permissions. If you only need to check a single permission, the :ref:`PermissionRequiredMixin` is a better choice.

    .. note::
        If you are using Django's built in auth system, ``superusers`` automatically have all permissions in your system.

::

    from django.views import TemplateView

    from braces import views


    class SomeProtectedView(views.LoginRequiredMixin,
                            views.MultiplePermissionsRequiredMixin,
                            TemplateView):

        #required
        permissions = {
            "all": ("blog.add_post", "blog.change_post"),
            "any": ("blog.delete_post", "user.change_user")
        }

The ``MultiplePermissionsRequiredMixin`` also offers a ``check_permssions`` method that should be overridden if you need custom permissions checking.


.. _GroupRequiredMixin:

GroupRequiredMixin
------------------

.. versionadded:: 1.2

The ``GroupRequiredMixin`` ensures that the requesting user is in the group or groups specified. This view mixin can handle multiple groups by setting the mandatory ``group_required`` attribute as a list or tuple.

    .. note::
        The mixin assumes you're using Django's default Group model and that your user model provides ``groups`` as a ManyToMany relationship.
        If this **is not** the case, you'll need to override ``check_membership`` in the mixin to handle your custom set up.

Standard Django Usage
^^^^^^^^^^^^^^^^^^^^^

::

    from django.views import TemplateView

    from braces.views import GroupRequiredMixin


    class SomeProtectedView(GroupRequiredMixin, TemplateView):

        #required
        group_required = u"editors"

Multiple Groups Possible Usage
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    from django.views import TemplateView
    
    from braces.views import GroupRequiredMixin


    class SomeProtectedView(GroupRequiredMixin, TemplateView):

        #required
        group_required = [u"editors", u"admins"]


Custom Group Usage
^^^^^^^^^^^^^^^^^^

::

    from django.views import TemplateView

    from braces.views import GroupRequiredMixin


    class SomeProtectedView(GroupRequiredMixin, TemplateView):

        #required
        group_required = u"editors"

        def check_membership(self, group):
            ...
            # Check some other system for group membership
            if user_in_group:
                return True
            else:
                return False


.. _UserPassesTestMixin:

UserPassesTestMixin
-------------------

.. versionadded:: 1.3.0

Mixin that reimplements the `user_passes_test`_ decorator. This is helpful for much more complicated cases than checking if user ``is_superuser`` (for example if their email is from a specific domain).

::

    from django.views import TemplateView

    from braces.views import UserPassesTestMixin


    class SomeUserPassView(UserPassesTestMixin, TemplateView):
        def test_func(self, user):
            return (user.is_staff and not user.is_superuser
                    and user.email.endswith(u"mydomain.com"))


.. _SuperuserRequiredMixin:

SuperuserRequiredMixin
----------------------

Another permission-based mixin. This is specifically for requiring a user to be a superuser. Comes in handy for tools that only privileged users should have access to.

::

    from django.views import TemplateView

    from braces import views


    class SomeSuperuserView(views.LoginRequiredMixin,
                            views.SuperuserRequiredMixin,
                            TemplateView):

        template_name = u"path/to/template.html"


.. _AnonymousRequiredMixin:

AnonymousRequiredMixin
----------------------

.. versionadded:: 1.4.0

Mixin that will redirect authenticated users to a different view. The default redirect is to
Django's `settings.LOGIN_REDIRECT_URL`_.


Static Examples
^^^^^^^^^^^^^^^

::

    from django.views import TemplateView

    from braces.views import AnonymousRequiredMixin


    class SomeView(AnonymousRequiredMixin, TemplateView):
        authenticated_redirect_url = u"/send/away/"


::

    from django.core.urlresolvers import reverse_lazy
    from django.views import TemplateView

    from braces.views import AnonymousRequiredMixin


    class SomeLazyView(AnonymousRequiredMixin, TemplateView):
        authenticated_redirect_url = reverse_lazy(u"view_url")


Dynamic Example
^^^^^^^^^^^^^^^

::

    from django.views import TemplateView

    from braces.views import AnonymousRequiredMixin


    class SomeView(AnonymousRequiredMixin, TemplateView):
        """ Redirect based on user level """
        def get_authenticated_redirect_url(self):
            if self.request.user.is_superuser:
                return u"/admin/"
            return u"/somewhere/else/"


.. _StaffuserRequiredMixin:

StaffuserRequiredMixin
----------------------

Similar to :ref:`SuperuserRequiredMixin`, this mixin allows you to require a user with ``is_staff`` set to ``True``.

::

    from django.views import TemplateView
    
    from braces import views


    class SomeStaffuserView(views.LoginRequiredMixin,
                            views.StaffuserRequiredMixin,
                            TemplateView):

        template_name = u"path/to/template.html"

.. _Daniel Sokolowski: https://github.com/danols
.. _code here: https://github.com/lukaszb/django-guardian/issues/48
.. _user_passes_test: https://docs.djangoproject.com/en/1.6/topics/auth/default/#django.contrib.auth.decorators.user_passes_test
.. _settings.LOGIN_REDIRECT_URL: https://docs.djangoproject.com/en/1.6/ref/settings/#login-redirect-url
