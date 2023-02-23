from typing import List, Dict

from django import forms
from django.core.exceptions import ImproperlyConfigured
from django.db import models
from django.utils.decorators import method_decorator
from django.views.decorators.csrf import csrf_exempt

from braces import mixins


class UserFormMixin:
    """Mixin for forms that stores the request.user as self.user"""

    def __init__(self, *args, **kwargs):
        if not issubclass(self.__class__, forms.Form):
            raise TypeError('UserFormMixin can only be used with forms')

        if 'user' in kwargs:
            self.user = kwargs.pop('user')
        super().__init__(*args, **kwargs)


class FormWithUserMixin:
    """Provides the view's form with the requesting user"""

    def get_form_kwargs(self):
        kwargs = super().get_form_kwargs()
        kwargs['user'] = self.request.user
        return kwargs

    def get_form_class(self):
        form_class = super().get_form_class()
        if issubclass(form_class, UserFormMixin):
            return form_class
        else:
            class FormWithUser(form_class, UserFormMixin):
                pass
            return FormWithUser


class CSRFExemptMixin:
    @method_decorator(csrf_exempt)
    def dispatch(self, request, *args, **kwargs):
        return super().dispatch(request, *args, **kwargs)


# Aliases
class CsrfExemptMixin(CSRFExemptMixin):
    pass


class SuccessURLRedirectMixin(mixins.RedirectMixin):
    success_url: str = None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.redirect_url = self.success_url

    def get_success_url(self) -> str:
        if self.success_url is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a success_url. Define "
                f"{name}.success_url, or override "
                f"{name}.get_success_url()."
            )
        return self.success_url

    def get_redirect_url(self) -> str:
        return self.get_success_url()


class MultipleFormsMixin:
    form_classes: Dict[str, forms.Form] = None
    initial: Dict[str, Dict] = {}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.get_form = self.get_forms

    def get_context_data(self, **kwargs) -> Dict:
        context = super().get_context_data(**kwargs)
        context["forms"] = self.get_forms()
        return context

    def get_form_classes(self) -> List:
        if self.form_classes is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a form_classes. Define "
                f"{name}.form_classes, or override "
                f"{name}.get_form_classes()."
            )

        if not isinstance(self.form_classes, dict):
            raise ImproperlyConfigured(
                f"{name}.form_classes must be a dict."
            )

        return self.form_classes

    def get_forms(self) -> Dict[str, forms.Form]:
        forms = {}
        for name, form_class in self.get_form_classes().items():
            forms[name] = form_class(**self.get_form_kwargs(name))
        return forms

    def get_form_kwargs(self, name) -> Dict:
        kwargs = {
            "prefix": name,
        }

        initial = self.get_initial()
        if name in initial:
            kwargs["initial"] = initial[name]

        if self.request.method in ("POST", "PUT", "PATCH"):
            kwargs["data"] = self.request.POST
            kwargs["files"] = self.request.FILES

        return kwargs

    def validate_forms(self) -> bool:
        forms = self.get_forms()
        return all(form.is_valid() for form in forms.values())

    def forms_valid(self) -> None:
        raise NotImplementedError

    def forms_invalid(self) -> None:
        raise NotImplementedError

    def post(self, request, *args, **kwargs):
        if self.validate_forms():
            return self.forms_valid()
        return self.forms_invalid()

    def put(self, request, *args, **kwargs):
        return self.post(request, *args, **kwargs)

    def patch(self, request, *args, **kwargs):
        return self.post(request, *args, **kwargs)


class MultipleModelFormsMixin(MultipleFormsMixin):
    instances: Dict[str, models.Model] = None

    def get_instances(self) -> Dict[str, models.Model]:
        if self.instances is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a instances. Define "
                f"{name}.instances, or override "
                f"{name}.get_instances()."
            )

        if not isinstance(self.instances, dict):
            raise ImproperlyConfigured(
                f"{name}.instances must be a dict."
            )

        return self.instances

    def get_form_kwargs(self, name) -> Dict:
        kwargs = super().get_form_kwargs(name)
        instances = self.get_instances()
        if name in instances:
            kwargs["instance"] = instances[name]
        return kwargs
