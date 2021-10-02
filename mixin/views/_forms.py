from functools import partial

from django.contrib import messages
from django.core.exceptions import ImproperlyConfigured
from django.utils.decorators import method_decorator
try:
    from django.utils.encoding import force_str as force_string
except ImportError:
    from django.utils.encoding import force_text as force_string
from django.utils.functional import Promise
from django.views.decorators.csrf import csrf_exempt
try:
    from django.urls import reverse
except ImportError:
    from django.core.urlresolvers import reverse

import six


class CsrfExemptMixin(object):
    """
    Exempts the view from CSRF requirements.

    NOTE:
        This should be the left-most mixin of a view.
    """

    @method_decorator(csrf_exempt)
    def dispatch(self, *args, **kwargs):
        return super(CsrfExemptMixin, self).dispatch(*args, **kwargs)


class UserFormKwargsMixin(object):
    """
    CBV mixin which puts the user from the request into the form kwargs.
    Note: Using this mixin requires you to pop the `user` kwarg
    out of the dict in the super of your form's `__init__`.
    """
    def get_form_kwargs(self):
        kwargs = super(UserFormKwargsMixin, self).get_form_kwargs()
        # Update the existing form kwargs dict with the request's user.
        kwargs.update({"user": self.request.user})
        return kwargs


class SuccessURLRedirectListMixin(object):
    """
    Simple CBV mixin which sets the success url to the list view of
    a given app. Set success_list_url as a class attribute of your
    CBV and don't worry about overloading the get_success_url.

    This is only to be used for redirecting to a list page. If you need
    to reverse the url with kwargs, this is not the mixin to use.
    """
    success_list_url = None  # Default the success url to none

    def get_success_url(self):
        # Return the reversed success url.
        if self.success_list_url is None:
            raise ImproperlyConfigured(
                '{0} is missing a success_list_url '
                'name to reverse and redirect to. Define '
                '{0}.success_list_url or override '
                '{0}.get_success_url().'.format(self.__class__.__name__))
        return reverse(self.success_list_url)


class _MessageAPIWrapper(object):
    """
    Wrap the django.contrib.messages.api module to automatically pass a given
    request object as the first parameter of function calls.
    """
    API = set([
        'add_message', 'get_messages',
        'get_level', 'set_level',
        'debug', 'info', 'success', 'warning', 'error',
    ])

    def __init__(self, request):
        for name in self.API:
            api_fn = getattr(messages.api, name)
            setattr(self, name, partial(api_fn, request))


class _MessageDescriptor(object):
    """
    A descriptor that binds the _MessageAPIWrapper to the view's
    request.
    """
    def __get__(self, instance, owner):
        return _MessageAPIWrapper(instance.request)


class MessageMixin(object):
    """
    Add a `messages` attribute on the view instance that wraps
    `django.contrib .messages`, automatically passing the current
    request object.
    """
    messages = _MessageDescriptor()


class FormValidMessageMixin(MessageMixin):
    """
    Mixin allows you to set static message which is displayed by
    Django's messages framework through a static property on the class
    or programmatically by overloading the get_form_valid_message method.
    """
    form_valid_message = None  # Default to None

    def get_form_valid_message(self):
        """
        Validate that form_valid_message is set and is either a
        unicode or str object.
        """
        if self.form_valid_message is None:
            raise ImproperlyConfigured(
                '{0}.form_valid_message is not set. Define '
                '{0}.form_valid_message, or override '
                '{0}.get_form_valid_message().'.format(self.__class__.__name__)
            )

        if not isinstance(self.form_valid_message,
                          (six.string_types, six.text_type, Promise)):
            raise ImproperlyConfigured(
                '{0}.form_valid_message must be a str or unicode '
                'object.'.format(self.__class__.__name__)
            )

        return force_string(self.form_valid_message)

    def form_valid(self, form):
        """
        Call the super first, so that when overriding
        get_form_valid_message, we have access to the newly saved object.
        """
        response = super(FormValidMessageMixin, self).form_valid(form)
        self.messages.success(self.get_form_valid_message(),
                              fail_silently=True)
        return response

    def delete(self, *args, **kwargs):
        response = super(FormValidMessageMixin, self).delete(*args, **kwargs)
        self.messages.success(self.get_form_valid_message(),
                              fail_silently=True)
        return response


class FormInvalidMessageMixin(MessageMixin):
    """
    Mixin allows you to set static message which is displayed by
    Django's messages framework through a static property on the class
    or programmatically by overloading the get_form_invalid_message method.
    """
    form_invalid_message = None

    def get_form_invalid_message(self):
        """
        Validate that form_invalid_message is set and is either a
        unicode or str object.
        """
        if self.form_invalid_message is None:
            raise ImproperlyConfigured(
                '{0}.form_invalid_message is not set. Define '
                '{0}.form_invalid_message, or override '
                '{0}.get_form_invalid_message().'.format(
                    self.__class__.__name__))

        if not isinstance(self.form_invalid_message,
                          (six.string_types, six.text_type, Promise)):
            raise ImproperlyConfigured(
                '{0}.form_invalid_message must be a str or unicode '
                'object.'.format(self.__class__.__name__))

        return force_string(self.form_invalid_message)

    def form_invalid(self, form):
        response = super(FormInvalidMessageMixin, self).form_invalid(form)
        self.messages.error(self.get_form_invalid_message(),
                            fail_silently=True)
        return response


class FormMessagesMixin(FormValidMessageMixin, FormInvalidMessageMixin):
    """
    Mixin is a shortcut to use both FormValidMessageMixin and
    FormInvalidMessageMixin.
    """
    pass
