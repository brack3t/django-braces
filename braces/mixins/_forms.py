from django import forms
from django.utils.decorators import method_decorator
from django.views.decorators.csrf import csrf_exempt


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