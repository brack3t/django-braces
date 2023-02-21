from django import forms
from django.core.exceptions import ImproperlyConfigured
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
