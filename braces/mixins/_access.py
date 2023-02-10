import inspect
from typing import Union
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.auth.views import redirect_to_login, logout_then_login
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django import http


class RequestPassesTest:
    request_test = None

    def dispatch(self, request, *args, **kwargs):
        test_method = self.get_test_method()

        if not test_method():
            return self.handle_test_failure()

        return super().dispatch(request, *args, **kwargs)

    def get_test_method(self):
        if self.request_test is None:
            raise ImproperlyConfigured(
                "{0} is missing the request_test method. Define {0}.request_test or "
                "override {0}.get_request_test().".format(self.__class__.__name__)
            )
        return getattr(self, self.request_test)

    def handle_test_failure(self):
        raise PermissionDenied

class BaseAccessMixin(RequestPassesTest):
    login_url: str = None
    redirect_field_name: str = REDIRECT_FIELD_NAME
    raise_exception: bool = False
    redirect_unauthenticated_users: bool = True
    request_test = "test_method"

    def get_login_url(self) -> str:
        """Returns the URL for the login page"""
        try:
            login_url = settings.LOGIN_URL
        except AttributeError:
            login_url = self.login_url

        if not login_url:
            raise ImproperlyConfigured(
                "Define {0}.login_url or settings.LOGIN_URL or "
                "override {0}.get_login_url().".format(self.__class__.__name__)
            )
        return login_url

    def get_redirect_field_name(self) -> str:
        """Returns the query string field name for the redirection URL"""
        if self.redirect_field_name is None:
            raise ImproperlyConfigured(
                "{0} is missing the redirect_field_name. "
                "Define {0}.redirect_field_name or "
                "override {0}.get_redirect_field_name().".format(self.__class__.__name__)
            )
        return self.redirect_field_name

    def test_method(self):
        return self.request.user.is_authenticated

    def handle_test_failure(self) -> PermissionDenied | http.HttpResponse:
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
            self.get_redirect_field_name()
        )


class SuperuserRequiredMixin(BaseAccessMixin):
    """Require the user to be authenticated and a superuser"""
    request_test = "test_superuser"

    def test_superuser(self):
        user = getattr(self.request, "user", None)
        if user is not None:
            return user.is_authenticated and user.is_superuser
        return False