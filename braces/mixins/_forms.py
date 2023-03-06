"""Mixins for form-using views. One mixin for forms."""
from __future__ import annotations  # pylint: disable=unused-variable

from django import forms
from django.core.exceptions import ImproperlyConfigured
from django.db import models
from django.http import HttpResponse
from django.utils.decorators import method_decorator
from django.views.decorators.csrf import csrf_exempt


__all__ = [  # pylint: disable=unused-variable
    "UserFormMixin",
    "FormWithUserMixin",
    "CSRFExemptMixin",
    "MultipleFormsMixin",
    "MultipleModelFormsMixin",
]


class UserFormMixin:  # pylint: disable=too-few-public-methods
    """Mixin for Forms/ModelForms that will store the request.user
    as self.user"""

    def __init__(self: forms.Form, *args, **kwargs):
        if not issubclass(self.__class__, forms.Form):
            raise TypeError("UserFormMixin can only be used with forms")

        if "user" in kwargs:
            self.user = kwargs.pop("user")
        super().__init__(*args, **kwargs)


class FormWithUserMixin:
    """Provides the view's form with the requesting user"""

    def get_form_kwargs(self):
        """Inject the request.user into the form's kwargs"""
        kwargs = super().get_form_kwargs()
        kwargs["user"] = self.request.user
        return kwargs

    def get_form_class(self):
        """Get the form class or wrap it with UserFormMixin"""
        form_class = super().get_form_class()
        if issubclass(form_class, UserFormMixin):
            return form_class

        # pylint: disable-next=too-few-public-methods, missing-class-docstring
        class FormWithUser(UserFormMixin, form_class):
            __doc__ = form_class.__doc__

        return FormWithUser


class CSRFExemptMixin:  # pylint: disable=too-few-public-methods
    """Exempts the view from CSRF requirements"""

    @method_decorator(csrf_exempt)
    def dispatch(self, request, *args, **kwargs):
        """Dispatch the exempted request"""

        return super().dispatch(request, *args, **kwargs)


CsrfExemptMixin = CSRFExemptMixin  # pylint: disable=unused-variable


class MultipleFormsMixin:
    """Provides a view with the ability to handle multiple Forms"""

    form_classes: dict[str, forms.Form] = None
    initial: dict[str, dict] = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.get_form = self.get_forms

    def get_context_data(self, **kwargs) -> dict:
        """Add the forms to the context"""
        context = super().get_context_data(**kwargs)
        context["forms"] = self.get_forms()
        return context

    def get_form_classes(self) -> list:
        """Get the form classes to use in this view"""
        if self.form_classes is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a form_classes. Define "
                f"{name}.form_classes, or override "
                f"{name}.get_form_classes()."
            )

        if not isinstance(self.form_classes, dict):
            raise ImproperlyConfigured(f"{name}.form_classes must be a dict.")

        return self.form_classes

    def get_forms(self) -> dict[str, forms.Form]:
        """Instantiates the forms with their kwargs"""
        _forms = {}
        for name, form_class in self.get_form_classes().items():
            _forms[name] = form_class(**self.get_form_kwargs(name))
        return _forms

    def get_form_kwargs(self, name) -> dict:
        """Add common kwargs to the form"""
        kwargs = {
            "prefix": name,  # all forms get a prefix
        }

        initial = self.get_initial()
        if name in initial:
            kwargs["initial"] = initial[name]  # use the form's initial data

        if self.request.method in {"POST", "PUT", "PATCH"}:
            # Attach the request's POST data and any files to the form
            kwargs["data"] = self.request.POST
            kwargs["files"] = self.request.FILES

        return kwargs

    def validate_forms(self) -> bool:
        """Validate all forms using their own .is_valid() method"""
        _forms = self.get_forms()
        return all(f.is_valid() for f in _forms.values())

    def forms_valid(self) -> HttpResponse:
        """Called when all forms are valid. Should return an HttpResponse"""
        raise NotImplementedError

    def forms_invalid(self) -> HttpResponse:
        """Called when any form is invalid. Should return an HttpResponse"""
        raise NotImplementedError

    # pylint: disable-next=unused-argument
    def post(self, request, *args, **kwargs):
        """Handle POST requests: validate and run appropriate handler"""
        if self.validate_forms():
            return self.forms_valid()
        return self.forms_invalid()

    def put(self, request, *args, **kwargs):
        """Process PUT requests like POSTs"""
        return self.post(request, *args, **kwargs)

    def patch(self, request, *args, **kwargs):
        """Process PATCH requests like POSTs"""
        return self.post(request, *args, **kwargs)


# pylint: disable-next=abstract-method
class MultipleModelFormsMixin(MultipleFormsMixin):
    """Provides a view with the ability to handle multiple ModelForms"""

    instances: dict[str, models.Model] = None

    def get_instances(self) -> dict[str, models.Model]:
        """Which instances should be used for each form?"""
        if self.instances is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a instances. Define "
                f"{name}.instances, or override "
                f"{name}.get_instances()."
            )

        if not isinstance(self.instances, dict):
            raise ImproperlyConfigured(f"{name}.instances must be a dict.")

        return self.instances

    def get_form_kwargs(self, name) -> dict:
        """Add the instance to the form if needed"""
        kwargs = {
            "prefix": name,  # all forms get a prefix
        }

        initial = self.get_initial()
        if name in initial:
            kwargs["initial"] = initial[name]  # use the form's initial data

        if self.request.method in {"POST", "PUT", "PATCH"}:
            # Attach the request's POST data and any files to the form
            kwargs["data"] = self.request.POST
            kwargs["files"] = self.request.FILES

        instances = self.get_instances()
        if name in instances:
            kwargs["instance"] = instances[name]

        return kwargs
