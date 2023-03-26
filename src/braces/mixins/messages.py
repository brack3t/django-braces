from functools import partial

from django.contrib import messages
from django.core.exceptions import ImproperlyConfigured
from django.utils.functional import Promise

__all__ = [
    "MessagesMixin",
    "FormValidMessageMixin",
    "FormInvalidMessageMixin",
    "FormMessagesMixin",
]


class _MessageHelper:
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

    def __init__(self, request):
        for name in self.API:
            api_fn = getattr(messages.api, name)
            setattr(self, name, partial(api_fn, request))


class _MessageDescriptor:
    """
    A descriptor that gives messages access to the request.
    """

    def __get__(self, instance, owner):
        """Use the custom MessageHelper, with the request"""
        if instance is None:
            return self
        return _MessageHelper(instance.request)

    def __set__(self, instance, value):
        """`messages` is a read-only attribute."""
        raise AttributeError("Cannot set the 'messages' attribute.")

    def __delete__(self, instance):
        """`messages` is a read-only attribute."""
        raise AttributeError("Cannot delete the 'messages' attribute.")


class MessagesMixin:
    """A mixin that provides access to the messages API."""

    messages = _MessageDescriptor()


class FormValidMessageMixin(MessagesMixin):
    """Automatically add a message when a form is valid."""

    form_valid_message: str = None

    def get_form_valid_message(self):
        """What message should be added?"""
        name = self.__class__.__name__
        if not getattr(self, "form_valid_message", None):
            raise ImproperlyConfigured(
                f"{name}.form_valid_message is not set. "
                f"Define {name}.form_valid_message, or "
                f"override {name}.get_form_valid_message()."
            )
        if self.form_valid_message is None:
            raise ImproperlyConfigured(
                f"{name}.form_valid_message is not set. "
                f"Define {name}.form_valid_message, or "
                f"override {name}.get_form_valid_message()."
            )

        if not isinstance(self.form_valid_message, (str, Promise)):
            raise ImproperlyConfigured(f"{name}.form_valid_message must be a str.")

        return self.form_valid_message

    def form_valid(self, form):
        """Add the message when the form is valid."""
        response = super().form_valid(form)
        self.messages.success(self.get_form_valid_message(), fail_silently=True)
        return response

    def delete(self, *args, **kwargs):
        """Add the message for deletes, too."""
        response = super().delete(*args, **kwargs)
        self.messages.success(self.get_form_valid_message(), fail_silently=True)
        return response


class FormInvalidMessageMixin(MessagesMixin):
    """Automatically add a message when a form is invalid."""

    form_invalid_message: str = None

    def get_form_invalid_message(self):
        """What message should be added?"""
        name = self.__class__.__name__

        if not getattr(self, "form_invalid_message", None):
            raise ImproperlyConfigured(
                f"{name}.form_invalid_message is not set. "
                f"Define {name}.form_invalid_message, or "
                f"override {name}.get_form_invalid_message()."
            )

        if self.form_invalid_message is None:
            raise ImproperlyConfigured(
                f"{name}.form_invalid_message is not set. "
                f"Define {name}.form_invalid_message, or "
                f"override {name}.get_form_invalid_message()."
            )

        if not isinstance(self.form_invalid_message, (str, Promise)):
            raise ImproperlyConfigured(f"{name}.form_invalid_message must be a str.")

        return self.form_invalid_message

    def form_invalid(self, form):
        """Add the message when the form is invalid."""
        response = super().form_invalid(form)
        self.messages.error(self.get_form_invalid_message(), fail_silently=True)
        return response


class FormMessagesMixin(FormValidMessageMixin, FormInvalidMessageMixin):
    """Attach messages for both valid and invalid forms."""

    ...
