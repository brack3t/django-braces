import inspect
from typing import Union
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.auth.views import redirect_to_login, logout_then_login
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django import http


class UserPassesTest:
    test_function = None


class BaseAccessMixin:
    login_url: str = None
    redirect_field_name: str = REDIRECT_FIELD_NAME
    raise_exception: bool = False

    def __init__(self, *args, **kwargs):
        self._class_name = self.__class__.__name__
        super().__init__(*args, *kwargs)

    def get_login_url(self) -> str:
        """Returns the URL for the login page"""
        try:
            login_url = settings.LOGIN_URL
        except AttributeError:
            login_url = self.login_url

        if not login_url:
            raise ImproperlyConfigured(
                f"Define {self._class_name}.login_url or settings.LOGIN_URL or "
                f"override {self._class_name}.get_login_url()."
            )
        return login_url

    def get_redirect_field_name(self) -> str:
        """Returns the query string field name for the redirection URL"""
        if self.redirect_field_name is None:
            raise ImproperlyConfigured(
                f"{self._class_name} is missing the redirect_field_name. "
                f"Define {self._class_name}.redirect_field_name or "
                f"override {self._class_name}.get_redirect_field_name()."
            )
        return self.redirect_field_name

    def handle_no_permission(self) -> PermissionDenied | http.HttpResponse:
        """User lacks permission, should we redirect or raise an exception?"""
        # redirect without an exception
        if not self.raise_exception:
            return self.redirect()

        # redirect unauthenticated users to login
        if (self.redirect_unauthenticated_users and
                not self.request.user.is_authenticated):
            return self.redirect()

        # if self.raise_exception is an exception, raise it
        if (inspect.isclass(self.raise_exception) and
                issubclass(self.raise_exception, Exception)):
            raise self.raise_exception

        # if self.raise_exception is a callable, call it
        if callable(self.raise_exception):
            if isinstance(
                response := self.raise_exception(self.request),
                (http.HttpResponse, http.StreamingHttpResponse)
            ):
                return response

        # raise the default exception
        raise PermissionDenied

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return redirect_to_login(
            self.request.get_full_path(),
            self.get_login_url(),
            self.get_redirect_field_name
        )

        