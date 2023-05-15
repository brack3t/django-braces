"""Mixins related to Django's messages framework."""

from functools import partial

from django import forms
from django.contrib import messages
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpRequest, HttpResponse
from django.utils.functional import Promise

__all__ = [
    "MessagesMixin",
    "FormValidMessageMixin",
    "FormInvalidMessageMixin",
    "FormMessagesMixin",
]


class MessageHelper:
    """An interface to the `django.contrib.messages` API."""

    API = set(
        [
            "add_message",
            "get_messages",
            "get_level",
            "set_level",
            "debug",
            "info",
            "success",
            "warning",
            "error",
            "MessageFailure",
        ]
    )

    def __init__(self, request: HttpRequest) -> None:
        for name in self.API:
            api_fn = getattr(messages.api, name)
            setattr(self, name, partial(api_fn, request))


class MessageDescriptor:
    """A descriptor that gives a message access to the request."""

    def __get__(self, instance, *args, **kwargs) -> MessageHelper:
        """Use the custom MessageHelper, with the request."""
        if instance is None:
            return self
        return MessageHelper(instance.request)

    def __set__(self, *args, **kwargs) -> AttributeError:
        """`messages` is a read-only attribute."""
        _err_msg = "Cannot set the `messages` attribute."
        raise AttributeError(_err_msg)

    def __delete__(self, *args, **kwargs) -> AttributeError:
        """`messages` is a read-only attribute."""
        _err_msg = "Cannot delete the `messages` attribute."
        raise AttributeError(_err_msg)


class MessagesMixin:
    """A mixin that provides access to the messages API."""

    messages = MessageDescriptor()


class FormValidMessageMixin(MessagesMixin):
    """Automatically add a message when a form is valid."""

    form_valid_message: str = None

    def get_form_valid_message(self) -> str:
        """Return the message for a valid form."""
        _class = self.__class__.__name__
        if (
            not getattr(self, "form_valid_message", None)
            or self.form_valid_message is None
        ):
            _err_msg = (
                f"{_class}.form_valid_message is not set. Define `{_class}."
                f"form_valid_message` or override `{_class}.get_form_valid_message`."
            )
            raise ImproperlyConfigured(_err_msg)

        if not isinstance(self.form_valid_message, (str, Promise)):
            _err_msg = f"{_class}.form_valid_message must be a str."
            raise ImproperlyConfigured(_err_msg)

        return self.form_valid_message

    def form_valid(self, form: forms.Form) -> HttpResponse:
        """Add the message when the form is valid."""
        response = super().form_valid(form)
        self.messages.success(self.get_form_valid_message(), fail_silently=True)
        return response

    def delete(self, *args, **kwargs) -> HttpResponse:
        """Add the message for deletes, too."""
        response = super().delete(*args, **kwargs)
        self.messages.success(self.get_form_valid_message(), fail_silently=True)
        return response


class FormInvalidMessageMixin(MessagesMixin):
    """Automatically add a message when a form is invalid."""

    form_invalid_message: str = None

    def get_form_invalid_message(self) -> str:
        """Return the message for an invalid form."""
        _class = self.__class__.__name__

        if (
            not getattr(self, "form_invalid_message", None)
            or self.form_invalid_message is None
        ):
            _err_msg = (
                f"{_class}.form_invalid_message is not set. Define `{_class}."
                f"form_invalid_message` or override `{_class}.get_form_invalid_message`."
            )
            raise ImproperlyConfigured(_err_msg)

        if not isinstance(self.form_invalid_message, (str, Promise)):
            _err_msg = f"{_class}.form_invalid_message must be a str."
            raise ImproperlyConfigured(_err_msg)

        return self.form_invalid_message

    def form_invalid(self, form: forms.Form) -> HttpResponse:
        """Add the message when the form is invalid."""
        response = super().form_invalid(form)
        self.messages.error(self.get_form_invalid_message(), fail_silently=True)
        return response


class FormMessagesMixin(FormValidMessageMixin, FormInvalidMessageMixin):
    """Attach messages for both valid and invalid forms."""

    ...
