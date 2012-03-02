Here are the generic mixins we've been using a lot lately. If you have any that you find useful, feel free to send them to us in a pull request. Please include example usage.

.. contents:: Mixins
    :depth: 1
    :class: well


LoginRequiredMixin
==================

This mixin is rather simple and is generally the first inherited class in any of our views. If we don't have an authenticated user 
there's no need to go any further. If you've used Django before you are probably familiar with the ``login_required`` decorator. 
All we are doing here is requiring a user to be authenticated to be able to get to this view.

While this doesn't look like much, it frees us up from having to manually overload the dispatch method on every single view that 
requires a user to be authenticated. If that's all that is needed on this view, we just saved 3 lines of code. Example usage below.

::

    from django.views.generic import TemplateView

    from braces.views import LoginRequiredMixin


    class SomeSecretView(LoginRequiredMixin, TemplateView):
        template_name = "path/to/template.html"

        def get(self, request):
            return self.render_to_response({})


PermissionRequiredMixin
=======================

This mixin was originally written, I believe, by `Daniel Sokolowski`_ (`code here`_), but we have updated it to eliminate an unneeded render if the permissions check fails.

The permission required mixin has been very handy for our client's custom CMS. Again, rather than overloading the 
dispatch method manually on every view that needs to check for the existence of a permission, we inherit this class 
and set the ``permission_required`` class attribute on our view. If you don't specify ``permission_required`` on 
your view, an ``ImproperlyConfigured`` exception is raised reminding you that you haven't set it.

The one limitation of this mixin is that it can **only** accept a single permission. It would need to be modified to 
handle more than one. We haven't needed that yet, so this has worked out well for us.

In our normal use case for this mixin, ``LoginRequiredMixin`` comes first, then the ``PermissionRequiredMixin``. If we 
don't have an authenticated user, there is no sense in checking for any permissions.

    .. role:: info-label
        :class: "label label-info"

    :info-label:`note` If you are using Django's built in auth system, ``superusers`` automatically have all permissions in your system.

::

    from braces.views import LoginRequiredMixin, PermissionRequiredMixin


    class SomeProtectedView(LoginRequiredMixin, PermissionRequiredMixin, TemplateView):
        permission_required = "auth.change_user"
        template_name = "path/to/template.html"

SuperuserRequiredMixin
======================

Another permission-based mixin. This is specifically for requiring a user to be a superuser. Comes in handy for tools that only privileged 
users should have access to.

::

    from braces.views import LoginRequiredMixin, SuperuserRequiredMixin


    class SomeSuperuserView(LoginRequiredMixin, SuperuserRequiredMixin, TemplateView):
        template_name = "path/to/template.html"

UserFormKwargsMixin
===================

In our clients CMS, we have a lot of form-based views that require a user to be passed in for permission-based form tools. For example, 
only superusers can delete or disable certain objects. To custom tailor the form for users, we have to pass that user instance into the form 
and based on their permission level, change certain fields or add specific options within the forms ``__init__`` method.

This mixin automates the process of overloading the ``get_form_kwargs`` (this method is available in any generic view which handles a form) method 
and stuffs the user instance into the form kwargs. We can then pop the user off in the form and do with it what we need. **Always** remember 
to pop the user from the kwargs before calling ``super`` on your form, otherwise the form gets an unexpected keyword argument and everything 
blows up. Example usage:

::

    from django.views.generic import CreateView

    from braces.views import LoginRequiredMixin, UserFormKwargsMixin
    from next.example import UserForm


    class SomeSecretView(LoginRequiredMixin, UserFormKwargsMixin,
        TemplateView):

        form_class = UserForm
        model = User
        template_name = "path/to/template.html"

This obviously pairs very nicely with the following ``Form`` mixin.


UserKwargModelFormMixin
=======================

The ``UserKwargModelFormMixin`` is a new form mixin we just implemented this week to go along with our ``UserFormKwargsMixin``. 
This becomes the first inherited class of our forms that receive the user keyword argument. With this mixin, we have automated 
the popping off of the keyword argument in our form and no longer have to do it manually on every form that works this way. 
While this may be overkill for a weekend project, for us, it speeds up adding new features. Example usage:

::

    from braces.forms import UserKwargModelFormMixin


    class UserForm(UserKwargModelFormMixin, forms.ModelForm):
        class Meta:
            model = User

        def __init__(self, *args, **kwargs):
            super(UserForm, self).__init__(*args, **kwargs):

            if not self.user.is_superuser:
                del self.fields["group"]


SuccessURLRedirectListMixin
===========================

The ``SuccessURLRedirectListMixin`` is a bit more tailored to how we handle CRUD_ within our CMS. Our CMS's workflow, by design, 
redirects the user to the ``ListView`` for whatever model they are working with, whether they are creating a new instance, editing 
an existing one or deleting one. Rather than having to override ``get_success_url`` on every view, we simply use this mixin and pass it 
a reversible route name. Example:

::

    # urls.py
    url(r"^users/$", UserListView.as_view(), name="cms_users_list"),

    # views.py
    from braces.views import (LoginRequiredMixin, PermissionRequiredMixin,
        SuccessURLRedirectListMixin)


    class UserCreateView(LoginRequiredMixin, PermissionRequiredMixin,
        SuccessURLRedirectListMixin, CreateView):

        form_class = UserForm
        model = User
        permission_required = "auth.add_user"
        success_list_url = "cms_users_list"
        ...


SetHeadlineMixin
================

The ``SetHeadlineMixin`` is a newer edition to our client's CMS. It allows us to *statically* or *programmatically* set the headline of any 
of our views. We like to write as few templates as possible, so a mixin like this helps us reuse generic templates. Its usage is amazingly 
straightforward and works much like Django's built-in ``get_queryset`` method. This mixin has two ways of being used.

Static Example
--------------

::

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        headline = "This is our headline"
        template_name = "path/to/template.html"


Dynamic Example
---------------

::

    from datetime import date

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        template_name = "path/to/template.html"

        def get_headline(self):
            return u"This is our headline for %s" % date.today().isoformat()

In both usages, in the template, just print out ``{{ headline }}`` to show the generated headline.


CreateAndRedirectToEditView
===========================

Mostly used for CRUD, where you're going to create an object and then move direct to the update view for that object. Your URL for the update view has to accept a PK for the object.

::

    # urls.py
    ...
    url(r"^users/create/$", UserCreateView.as_view(), name="cms_users_create"),
    url(r"^users/edit/(?P<pk>\d+)/$", UserUpdateView.as_view(), name="cms_users_update"),
    ...

    # views.py
    from braces.views import CreateAndRedirectToEditView


    class UserCreateView(CreateAndRedirectToEditView, CreateView):
        model = User
        ...


.. _Daniel Sokolowski: https://github.com/danols
.. _code here: https://github.com/lukaszb/django-guardian/issues/48
.. _CRUD: http://en.wikipedia.org/wiki/Create,_read,_update_and_delete
