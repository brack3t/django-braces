Form Mixins
===========

These mixins are all associated with forms and form processing.

.. contents::

.. _UserFormMixin:

UserFormMixin
-------------

.. note::
    This mixin is for use on a form.

This mixin will automatically pop ``request.user`` out of the keyword
arguments for the form and assign it to ``self.user``. This is useful
when you need access to the requesting user.

::

    from django import forms

    from braces.mixins import UserFormMixin

    class RequestUserForm(UserFormMixin, forms.Form):
        ...


.. _FormWithUserMixin:

FormWithUserMixin
-----------------

A companion mixin to go along with :ref:`UserFormMixin`. This mixin will
cause the view to automatically include the ``request.user`` in the form's
keyword arguments.

::

    from django.views.generic import CreateView

    from braces.mixins import FormWithUserMixin
    from next.example import UserForm


    class SomeSecretView(FormWithUserMixin, CreateView):
        form_class = UserForm
        model = User
        template_name = "path/to/template.html"


.. _CSRFExemptMixin:

CSRFExemptMixin
---------------

Mixin to easily opt a view out of requiring a CSRF token.

.. note::
    This mixin is aliases as ``CsrfExemptMixin`` for backwards
    compatibility and personal preference.

::

    from django.views.generic import UpdateView

    from braces.mixins import CSRFExemptMixin


    class UpdateProfileView(CsrfExemptMixin, UpdateView):
        ...

.. _MultipleFormsMixin:

MultipleFormsMixin
------------------

Often views need to handle multiple forms on the same request. While
this isn't a difficult thing to do with Django, it does require some
knowledge and code. This mixin makes it much easier.

::

    from django.views.generic import UpdateView

    from braces.mixins import MultipleFormsMixin

    from next.example import UserForm, ProfileForm


    class UserProfileView(MultipleFormsMixin, UpdateView):
        """Update the user's details and their profile."""

        form_classes = {
            "user_form": UserForm,
            "profile_form": ProfileForm,
        }

        initial = {"user_form": {"first_name": "Sue"}}

This mixin adds two new methods to the view:

* ``forms_valid`` is called when all forms are valid.
* ``forms_invalid`` is called when any form is invalid.

Both of these raise ``NotImplementedError`` by default, so you must
define the consequences of your forms yourself.


.. _MultipleModelFormsMixin:

MultipleModelFormsMixin
-----------------------

This mixin is a subclass of :ref:`MultipleFormsMixin` that is designed to
work with model forms. It adds another attribute, ``instances``, which
is filled out just like ``initial`` in the previous example. It's common,
however, to have to programmatically provide instances. That is
demonstrated below.

::

    from django.views.generic import UpdateView

    from braces.mixins import MultipleModelFormsMixin

    from next.example import UserForm, ProfileForm


    class UserProfileView(MultipleModelFormsMixin, UpdateView):
        form_classes = {
            "user_form": UserForm,
            "profile_form": ProfileForm,
        }

        def get_instances(self):
            """Get the instances for the forms."""
            return {
                "user_form": self.request.user,
                "profile_form": self.request.user.profile,
            }
