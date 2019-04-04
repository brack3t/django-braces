Form Mixins
===========

All of these mixins, with one exception, modify how forms are handled within views. The ``UserKwargModelFormMixin`` is a mixin for use in forms to auto-pop a ``user`` kwarg.

.. contents::

.. _CsrfExemptMixin:

CsrfExemptMixin
---------------

If you have Django's `CSRF protection`_ middleware enabled you can exempt views using the `csrf_exempt`_ decorator. This mixin exempts POST requests from the CSRF protection middleware without requiring that you decorate the ``dispatch`` method.

    .. note::

        This mixin should always be the left-most plugin.

::

    from django.views.generic import UpdateView

    from braces.views import LoginRequiredMixin, CsrfExemptMixin

    from profiles.models import Profile


    class UpdateProfileView(CsrfExemptMixin, LoginRequiredMixin, UpdateView):
        model = Profile


.. _UserFormKwargsMixin:

UserFormKwargsMixin
-------------------

A common pattern in Django is to have forms that are customized to a user. To custom tailor the form for users, you have to pass that user instance into the form and, based on their permission level or other details, change certain fields or add specific options within the forms ``__init__`` method.

This mixin automates the process of overloading the ``get_form_kwargs`` (this method is available in any generic view which handles a form) method and stuffs the user instance into the form kwargs. The user can then be ``pop()``\ ped off in the form. **Always** remember to pop the user from the kwargs before calling ``super()`` on your form, otherwise the form will get an unexpected keyword argument.

Usage
^^^^^

::

    from django.views.generic import CreateView

    from braces.views import LoginRequiredMixin, UserFormKwargsMixin
    from next.example import UserForm


    class SomeSecretView(LoginRequiredMixin, UserFormKwargsMixin, CreateView):
        form_class = UserForm
        model = User
        template_name = "path/to/template.html"

This obviously pairs very nicely with the following mixin.


.. _UserKwargModelFormMixin:

UserKwargModelFormMixin
-----------------------

The ``UserKwargModelFormMixin`` is a form mixin to go along with our :ref:`UserFormKwargsMixin`.
This becomes the first inherited class of our forms that receive the ``user`` keyword argument. With this mixin, the ``pop()``\ ping of the ``user`` is automated and no longer has to be done manually on every form that needs this behavior. 

Usage
^^^^^

::

    from braces.forms import UserKwargModelFormMixin


    class UserForm(UserKwargModelFormMixin, forms.ModelForm):
        class Meta:
            model = User

        def __init__(self, *args, **kwargs):
            super(UserForm, self).__init__(*args, **kwargs)

            if not self.user.is_superuser:
                del self.fields["group"]


.. _SuccessURLRedirectListMixin:

SuccessURLRedirectListMixin
---------------------------

The ``SuccessURLRedirectListMixin`` is a bit more tailored to how CRUD_ is often handled within CMSes. Many CMSes, by design, redirect the user to the ``ListView`` for whatever model they are working with, whether they are creating a new instance, editing an existing one, or deleting one. Rather than having to override ``get_success_url`` on every view, use this mixin and pass it a reversible route name. Example:

::

    # urls.py
    url(r"^users/$", UserListView.as_view(), name="users_list"),

    # views.py
    from django.views import CreateView

    from braces import views


    class UserCreateView(views.LoginRequiredMixin, views.PermissionRequiredMixin,
        views.SuccessURLRedirectListMixin, CreateView):

        form_class = UserForm
        model = User
        permission_required = "auth.add_user"
        success_list_url = "users_list"
        ...


.. _FormValidMessageMixin:

FormValidMessageMixin
---------------------

.. versionadded:: 1.2

The ``FormValidMessageMixin`` allows you to to *statically* or *programmatically* set a message to be returned using Django's `messages`_ framework when the form is valid. The returned message is controlled by the ``form_valid_message`` property which can either be set on the view or returned by the ``get_form_valid_message`` method. The message is not processed until the end of the ``form_valid`` method.

    .. warning::
        This mixin requires the Django `messages`_ app to be enabled.

    .. note::
        This mixin is designed for use with Django's generic form class-based views, e.g. ``FormView``, ``CreateView``, ``UpdateView``


Static Example
^^^^^^^^^^^^^^

::

    from django.utils.translation import ugettext_lazy as _
    from django.views.generic import CreateView

    from braces.views import FormValidMessageMixin


    class BlogPostCreateView(FormValidMessageMixin, CreateView):
        form_class = PostForm
        model = Post
        form_valid_message = _(u"Blog post created!")


Dynamic Example
^^^^^^^^^^^^^^^

::

    from django.views.generic import CreateView

    from braces.views import FormValidMessageMixin


    class BlogPostCreateView(FormValidMessageMixin, CreateView):
        form_class = PostForm
        model = Post

        def get_form_valid_message(self):
            return u"{0} created!".format(self.object.title)



.. _FormInvalidMessageMixin:

FormInvalidMessageMixin
-----------------------

.. versionadded:: 1.2

The ``FormInvalidMessageMixin`` allows you to to *statically* or *programmatically* set a message to be returned using Django's `messages`_ framework when the form is invalid. The returned message is controlled by the ``form_invalid_message`` property which can either be set on the view or returned by the ``get_form_invalid_message`` method. The message is not processed until the end of the ``form_invalid`` method.

    .. warning::
        This mixin requires the Django `messages`_ app to be enabled.

    .. note::
        This mixin is designed for use with Django's generic form class-based views, e.g. ``FormView``, ``CreateView``, ``UpdateView``

Static Example
^^^^^^^^^^^^^^

::

    from django.utils.translation import ugettext_lazy
    from django.views.generic import CreateView

    from braces.views import FormInvalidMessageMixin


    class BlogPostCreateView(FormInvalidMessageMixin, CreateView):
        form_class = PostForm
        model = Post
        form_invalid_message = _(u"Oh snap, something went wrong!")


Dynamic Example
^^^^^^^^^^^^^^^

::

    from django.utils.translation import ugettext_lazy as _
    from django.views.generic import CreateView

    from braces.views import FormInvalidMessageMixin


    class BlogPostCreateView(FormInvalidMessageMixin, CreateView):
        form_class = PostForm
        model = Post

        def get_form_invalid_message(self):
            return _(u"Some custom message")


.. _FormMessagesMixin:

FormMessagesMixin
-----------------

.. versionadded:: 1.2

``FormMessagesMixin`` is a convenience mixin which combines :ref:`FormValidMessageMixin` and :ref:`FormInvalidMessageMixin` since we commonly provide messages for both states (``form_valid``, ``form_invalid``).

    .. warning::
        This mixin requires the Django `messages`_ app to be enabled.

Static & Dynamic Example
^^^^^^^^^^^^^^^^^^^^^^^^

::

    from django.utils.translation import ugettext_lazy as _
    from django.views.generic import CreateView

    from braces.views import FormMessagesMixin


    class BlogPostCreateView(FormMessagesMixin, CreateView):
        form_class = PostForm
        form_invalid_message = _(u"Something went wrong, post was not saved")
        model = Post

        def get_form_valid_message(self):
            return u"{0} created!".format(self.object.title)


.. _CRUD: http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
.. _CSRF protection: https://docs.djangoproject.com/en/stable/ref/csrf/
.. _csrf_exempt: https://docs.djangoproject.com/en/stable/ref/csrf/#django.views.decorators.csrf.csrf_exempt
.. _messages: https://docs.djangoproject.com/en/stable/ref/contrib/messages/
