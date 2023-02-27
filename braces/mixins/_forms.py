from typing import Dict, List

from django import forms
from django.core.exceptions import ImproperlyConfigured
from django.db import models
from django.http import HttpResponse
from django.utils.decorators import method_decorator
from django.views.decorators.csrf import csrf_exempt


class UserFormMixin:
    """Mixin for Forms/ModelForms that will store the request.user as self.user"""

    def __init__(self, *args, **kwargs):
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
        else:

            class FormWithUser(UserFormMixin, form_class):
                pass

            return FormWithUser


@method_decorator(csrf_exempt, name="dispatch")
class CSRFExemptMixin:
    """Exempts the view from CSRF requirements"""

    def dispatch(self, request, *args, **kwargs):
        """Dispatch the exempted request"""

        return super().dispatch(request, *args, **kwargs)

    # @method_decorator(csrf_exempt)
    # def dispatch(self, request, *args, **kwargs):
    #     """Dispatch the exempted request"""

    #     return super().dispatch(request, *args, **kwargs)

    # def as_view(self, **initkwargs):
    #     """Return the view function"""
    #     view = super().as_view(**initkwargs)
    #     return csrf_exempt(view)


# Aliases
class CsrfExemptMixin(CSRFExemptMixin):
    """Exempts the view from CSRF requirements"""
    pass


class MultipleFormsMixin:
    """Provides a view with the ability to handle multiple Forms"""
    form_classes: Dict[str, forms.Form] = None
    initial: Dict[str, Dict] = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.get_form = self.get_forms

    def get_context_data(self, **kwargs) -> Dict:
        """Add the forms to the context"""
        context = super().get_context_data(**kwargs)
        context["forms"] = self.get_forms()
        return context

    def get_form_classes(self) -> List:
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

    def get_forms(self) -> Dict[str, forms.Form]:
        """Instantiates the forms with their kwargs"""
        forms = {}
        for name, form_class in self.get_form_classes().items():
            forms[name] = form_class(**self.get_form_kwargs(name))
        return forms

    def get_form_kwargs(self, name) -> Dict:
        """Add common kwargs to the form"""
        kwargs = {
            "prefix": name,  # all forms get a prefix
        }

        initial = self.get_initial()
        if name in initial:
            kwargs["initial"] = initial[name]  # use the form's initial data

        if self.request.method in ("POST", "PUT", "PATCH"):
            """Attach the request's POST data and any files to the form"""
            kwargs["data"] = self.request.POST
            kwargs["files"] = self.request.FILES

        return kwargs

    def validate_forms(self) -> bool:
        """Validate all forms using their own .is_valid() method"""
        forms = self.get_forms()
        return all(form.is_valid() for form in forms.values())

    def forms_valid(self) -> HttpResponse:
        """Called when all forms are valid. Should return an HttpResponse"""
        raise NotImplementedError

    def forms_invalid(self) -> HttpResponse:
        """Called when any form is invalid. Should return an HttpResponse"""
        raise NotImplementedError

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


class MultipleModelFormsMixin(MultipleFormsMixin):
    """Provides a view with the ability to handle multiple ModelForms"""
    instances: Dict[str, models.Model] = None

    def get_instances(self) -> Dict[str, models.Model]:
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

    def get_form_kwargs(self, name) -> Dict:
        """Add the instance to the form if needed"""
        assert self.instances
        kwargs = super().get_form_kwargs(name)
        instances = self.get_instances()
        if name in instances:
            kwargs["instance"] = instances[name]
        return kwargs
