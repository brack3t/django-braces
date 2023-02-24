import inspect
from datetime import timedelta
from typing import Dict, List, Union

from django import http
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.auth.views import logout_then_login, redirect_to_login
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now

from braces.mixins._redirects import RedirectMixin


class RequestPassesTest:
    request_test: Union[str, callable] = None

    def dispatch(self, request, *args, **kwargs):
        test_method = self.get_test_method()

        if not test_method():
            return self.handle_test_failure()

        return super().dispatch(request, *args, **kwargs)

    def get_test_method(self) -> callable:
        if self.request_test is None:
            raise ImproperlyConfigured(
                "{0} is missing the request_test method. Define {0}.request_test or "
                "override {0}.get_request_test().".format(
                    self.__class__.__name__
                )
            )

        try:
            method = getattr(self, self.request_test)
        except AttributeError:
            raise ImproperlyConfigured(
                "{0} is missing the request_test method. Define {0}.request_test or "
                "override {0}.get_request_test().".format(
                    self.__class__.__name__
                )
            )
        if not callable(method):
            raise ImproperlyConfigured(
                "{0}.{1} must be callable.".format(
                    self.__class__.__name__, self.request_test
                )
            )

        return method

    def handle_test_failure(self) -> Exception:
        raise PermissionDenied


class RedirectOnFailure(RedirectMixin, RequestPassesTest):
    login_url: str = None
    redirect_field_name: str = REDIRECT_FIELD_NAME
    raise_exception: bool = False
    redirect_unauthenticated_users: bool = True

    def get_login_url(self) -> str:
        """Returns the URL for the login page"""
        if self.login_url is None:
            try:
                login_url = settings.LOGIN_URL
            except AttributeError:
                login_url = self.login_url
        else:
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
                "override {0}.get_redirect_field_name().".format(
                    self.__class__.__name__
                )
            )
        return self.redirect_field_name

    def handle_test_failure(
        self,
    ) -> Union[PermissionDenied, http.HttpResponse]:
        """Test failed, should we redirect or raise an exception?"""
        # redirect without an exception
        if not self.raise_exception:
            return self.redirect()

        # redirect unauthenticated users to login
        if (
            self.redirect_unauthenticated_users
            and not self.request.user.is_authenticated
        ):
            return self.redirect()

        # if self.raise_exception is an exception, raise it
        if inspect.isclass(self.raise_exception) and issubclass(
            self.raise_exception, Exception
        ):
            raise self.raise_exception

        # if self.raise_exception is a callable, call it
        if callable(self.raise_exception):
            if isinstance(
                response := self.raise_exception(self.request),
                (http.HttpResponse, http.StreamingHttpResponse),
            ):
                return response

        # raise the default exception
        raise http.Http404

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return redirect_to_login(
            self.request.get_full_path(),
            self.get_login_url(),
            self.get_redirect_field_name(),
        )


class SuperuserRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated and a superuser"""

    request_test: str = "test_superuser"

    def test_superuser(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return user.is_authenticated and user.is_superuser
        return False


class StaffUserRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated and a staff user"""

    request_test: str = "test_staffuser"

    def test_staffuser(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return user.is_authenticated and user.is_staff
        return False


class GroupRequiredMixin(RedirectOnFailure):
    """Requires the user to be authenticated and a member of at least one of the specified group"""

    group_required: Union[str, List[str]] = None
    request_test: str = "check_groups"

    def get_group_required(self) -> List[str]:
        """Returns the group required"""
        if self.group_required is None:
            raise ImproperlyConfigured(
                "{0} is missing the group_required. "
                "Define {0}.group_required or "
                "override {0}.get_group_required().".format(
                    self.__class__.__name__
                )
            )
        if isinstance(self.group_required, str):
            return [self.group_required]
        return self.group_required

    def check_membership(self) -> bool:
        """Check if the user is a member of at least one of the required groups"""
        return bool(
            set(self.get_group_required()).intersection(
                [group.name for group in self.request.user.groups.all()]
            )
        )

    def check_groups(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return user.is_authenticated and self.check_membership()
        return False


class AnonymousRequiredMixin(RedirectOnFailure):
    """Require the user to be anonymous"""

    request_test: str = "test_anonymous"
    redirect_unauthenticated_users: bool = False

    def test_anonymous(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return not user.is_authenticated
        return True


class LoginRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated"""

    request_test: str = "test_authenticated"

    def test_authenticated(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return user.is_authenticated
        return False


class RecentLoginRequiredMixin(LoginRequiredMixin):
    """Require the user to be authenticated and have a recent login"""

    request_test: str = "test_recent_login"
    max_age: int = 1800  # 30 minutes

    def test_recent_login(self) -> bool:
        user = getattr(self.request, "user", None)
        if user is not None:
            return (
                user.is_authenticated
                and user.last_login > now() - timedelta(seconds=self.max_age)
            )
        return False

    def handle_test_failure(
        self,
    ) -> Union[PermissionDenied, http.HttpResponse]:
        return logout_then_login(self.request, self.get_login_url())


class PermissionRequiredMixin(RedirectOnFailure):
    permission_required: Union[str, Dict[str, List[str]]] = None
    request_test: str = "check_permissions"

    def get_permission_required(self) -> Union[str, Dict[str, List[str]]]:
        """Returns the permission required"""
        if self.permission_required is None:
            raise ImproperlyConfigured(
                "{0} is missing the permission_required. "
                "Define {0}.permission_required or "
                "override {0}.get_permission_required().".format(
                    self.__class__.__name__
                )
            )
        if isinstance(self.permission_required, str):
            return {"all": [self.permission_required]}
        return self.permission_required

    def check_permissions(self) -> bool:
        permissions = self.get_permission_required()
        _all = permissions.get("all", [])
        _any = permissions.get("any", [])

        if not getattr(self.request, "user", None):
            return False
        perms_all = self.request.user.has_perms(_all) or []
        perms_any = [self.request.user.has_perm(perm) for perm in _any]

        return any((perms_all, any(perms_any)))


class SSLRequiredMixin(RedirectOnFailure):
    """Require the user to be using SSL"""

    request_test: str = "test_ssl"
    redirect_to_ssl: bool = True

    def test_ssl(self) -> bool:
        if getattr(settings, "DEBUG", False):
            return True

        return self.request.is_secure()

    def handle_test_failure(
        self,
    ) -> Union[PermissionDenied, http.HttpResponse]:
        if self.redirect_to_ssl:
            current = self.request.build_absolute_uri(
                self.request.get_full_path()
            )
            secure = current.replace("http://", "https://")
            return http.HttpResponsePermanentRedirect(secure)
        return super().handle_test_failure()
