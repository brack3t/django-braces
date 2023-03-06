"""Mixins related to authorization and authentication"""
from __future__ import annotations  # pylint: disable=unused-variable

import inspect
from datetime import timedelta
from typing import Callable, Union

from django import http
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.auth.views import logout_then_login, redirect_to_login
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now

from braces.mixins._redirects import RedirectMixin

# pylint: disable-next=unused-variable
__all__ = [
    "PassesTest",
    "RedirectOnFailure",
    "SuperuserRequiredMixin",
    "StaffUserRequiredMixin",
    "GroupRequiredMixin",
    "AnonymousRequiredMixin",
    "LoginRequiredMixin",
    "RecentLoginRequiredMixin",
    "PermissionRequiredMixin",
    "SSLRequiredMixin",
]


class PassesTest:
    """Requires a test, usually of the request, to pass before the view
    is dispatched."""

    request_test: Union[str, Callable] = None

    def dispatch(self, request, *args, **kwargs):
        """Run the test method and dispatch the view if it passes."""
        test_method = self.get_test_method()

        if not test_method():
            return self.handle_test_failure()

        return super().dispatch(request, *args, **kwargs)  # pylint: disable=no-member

    def get_test_method(self) -> Callable:
        """What method should be used to test the request?
        Provide a callable object or a string that can be used to
        look up a callable
        """
        if self.request_test is None:
            class_name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{class_name} is missing the request_test method. "
                f"Define {class_name}.request_test or override "
                f"{class_name}.get_request_test()."
            )

        try:
            method = getattr(self, self.request_test)
        except AttributeError as exc:
            class_name = self.__class__.__name__

            raise ImproperlyConfigured(
                f"{class_name} is missing the request_test method. "
                f"Define {class_name}.request_test or "
                f"override {class_name}.get_request_test()."
            ) from exc

        if not callable(method):
            raise ImproperlyConfigured(
                f"{self.__class__.__name__}.{self.request_test} must be "
                "callable."
            )

        return method

    def handle_test_failure(self) -> Exception:
        """Test failed, raise an exception or redirect"""
        raise PermissionDenied


class RedirectOnFailure(RedirectMixin, PassesTest):
    """Redirects to a login page if the request test fails"""

    login_url: str = None
    redirect_field_name: str = REDIRECT_FIELD_NAME
    raise_exception: bool | Exception | Callable = False
    redirect_unauthenticated_users: bool = True

    def get_login_url(self) -> str:
        """Returns the URL for the login page"""
        if self.login_url is None:
            try:
                self.login_url = settings.LOGIN_URL
            except AttributeError as exc:
                name = self.__class__.__name__
                raise ImproperlyConfigured(
                    f"Define {name}.login_url or settings.LOGIN_URL or "
                    f"override {name}.get_login_url()."
                ) from exc
        return self.login_url

    def get_redirect_field_name(self) -> str:
        """Returns the query string field name for the redirection URL"""
        if self.redirect_field_name is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing the redirect_field_name. "
                f"Define {name}.redirect_field_name or "
                f"override {name}.get_redirect_field_name()."
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
            and not self.request.user.is_authenticated  # pylint: disable=no-member
        ):
            return self.redirect()

        # if self.raise_exception is an exception, raise it
        if inspect.isclass(self.raise_exception) and issubclass(
            self.raise_exception, Exception
        ):
            raise self.raise_exception  # pylint: disable=not-callable,raising-bad-type

        # if self.raise_exception is a callable, call it
        if callable(self.raise_exception):
            if isinstance(
                response := self.raise_exception(self.request),  # pylint: disable=not-callable
                (http.HttpResponse, http.StreamingHttpResponse),
            ):
                return response

        # raise the default exception
        raise http.Http404

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return redirect_to_login(
            self.request.get_full_path(),  # pylint: disable=no-member
            self.get_login_url(),
            self.get_redirect_field_name(),
        )


class SuperuserRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated and a superuser"""

    request_test: str = "test_superuser"

    def test_superuser(self) -> bool:
        """The user must be authenticated and a superuser"""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.is_superuser
        return False


class StaffUserRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated and a staff user"""

    request_test: str = "test_staffuser"

    def test_staffuser(self) -> bool:
        """The user must be authenticated and is_staff=True"""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.is_staff
        return False


class GroupRequiredMixin(RedirectOnFailure):
    """Requires the user to be authenticated and a member of at least
    one of the specified group"""

    group_required: Union[str, list[str]] = None
    request_test: str = "check_groups"

    def get_group_required(self) -> list[str]:
        """Returns the group(s) required"""
        if self.group_required is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing the group_required. "
                f"Define {name}.group_required or "
                f"override {name}.get_group_required()."
            )
        if isinstance(self.group_required, str):
            return [self.group_required]
        return self.group_required

    def check_membership(self) -> bool:
        """Check if the user is a member of at least one of the
        required groups"""
        return bool(
            set(self.get_group_required()).intersection(
                [group.name for group in self.request.user.groups.all()]
            )
        )

    def check_groups(self) -> bool:
        """The user must be authenticated and a member of the
        appropriate groups"""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and self.check_membership()
        return False


class AnonymousRequiredMixin(RedirectOnFailure):
    """Require the user to be anonymous"""

    request_test: str = "test_anonymous"
    redirect_unauthenticated_users: bool = False

    def test_anonymous(self) -> bool:
        """The user must be anonymous, non-authenticated
        No redirect to login as that wouldn't make sense."""
        if (user := getattr(self.request, "user", None)) is not None:
            return not user.is_authenticated
        return True


class LoginRequiredMixin(RedirectOnFailure):
    """Require the user to be authenticated"""

    request_test: str = "test_authenticated"

    def test_authenticated(self) -> bool:
        """The user must be authenticated"""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated
        return False


class RecentLoginRequiredMixin(LoginRequiredMixin):
    """Require the user to be authenticated and have a recent login"""

    request_test: str = "test_recent_login"
    max_age: int = 1800  # 30 minutes

    def test_recent_login(self) -> bool:
        """Make sure the user's login is recent enough"""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.last_login > now() - timedelta(
                seconds=self.max_age
            )
        return False

    def handle_test_failure(
        self,
    ) -> Union[PermissionDenied, http.HttpResponse]:
        """Logout the user and redirect to login"""
        return logout_then_login(self.request, self.get_login_url())


class PermissionRequiredMixin(RedirectOnFailure):
    """Require a user to have specific permission(s)"""

    permission_required: Union[str, dict[str, list[str]]] = None
    request_test: str = "check_permissions"

    def get_permission_required(self) -> Union[str, dict[str, list[str]]]:
        """Returns the permission(s) required"""
        if self.permission_required is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing the permission_required. "
                f"Define {name}.permission_required or "
                f"override {name}.get_permission_required()."
            )
        if isinstance(self.permission_required, str):
            return {"all": [self.permission_required]}
        return self.permission_required

    def check_permissions(self) -> bool:
        """Get permissions, make sure the user has them appropriately"""
        permissions = self.get_permission_required()
        _all = permissions.get("all", [])
        _any = permissions.get("any", [])

        if not getattr(self.request, "user", None):
            return False
        perms_all = self.request.user.has_perms(_all) or []
        perms_any = [self.request.user.has_perm(perm) for perm in _any]

        return any((perms_all, any(perms_any)))


class SSLRequiredMixin(RedirectOnFailure):
    """Require the request to be using SSL"""

    request_test: str = "test_ssl"
    redirect_to_ssl: bool = True

    def test_ssl(self) -> bool:
        """The request must be using SSL"""
        if getattr(settings, "DEBUG", False):
            return True

        return self.request.is_secure()

    def handle_test_failure(
        self,
    ) -> Union[PermissionDenied, http.HttpResponse]:
        """If not using SSL, redirect to the same URL but with HTTPS"""
        if self.redirect_to_ssl:
            current = self.request.build_absolute_uri(self.request.get_full_path())
            secure = current.replace("http://", "https://")
            return http.HttpResponsePermanentRedirect(secure)
        return super().handle_test_failure()
