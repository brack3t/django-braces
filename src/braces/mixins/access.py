"""Authentication and Authorization mixins."""

from __future__ import annotations

import typing
from datetime import timedelta

from django.conf import settings
from django.contrib.auth.views import logout_then_login
from django.core.exceptions import BadRequest, ImproperlyConfigured, PermissionDenied
from django.http import HttpResponsePermanentRedirect
from django.utils.timezone import now

from braces.mixins.redirects import RedirectOnFailureMixin

if typing.TYPE_CHECKING:  # pragma: no cover
    from typing import Any, Callable, Union

    from django.http import HttpRequest, HttpResponse, HttpResponseRedirect

__all__ = [
    "PassesTestMixin",
    "SuperuserRequiredMixin",
    "StaffUserRequiredMixin",
    "GroupRequiredMixin",
    "AnonymousRequiredMixin",
    "LoginRequiredMixin",
    "RecentLoginRequiredMixin",
    "PermissionRequiredMixin",
    "SSLRequiredMixin",
]


class PassesTestMixin(RedirectOnFailureMixin):
    """The view is not dispatched unless a test method passes.

    Executes a test function before `View.dispatch` is called. On failure,
    another method is called to handle whatever comes next.
    """

    dispatch_test: str = None

    def dispatch(
        self,
        request: HttpRequest,
        *args: tuple[Any],
        **kwargs: dict[Any, Any],
    ) -> HttpResponse:
        """Run the test method and dispatch the view if it passes."""
        test_method = self.get_test_method()

        if not test_method():
            return self.handle_test_failure()

        return super().dispatch(request, *args, **kwargs)

    def get_test_method(self) -> Callable:
        """Find the method to test the request with.

        Provide a callable object or a string that can be used to
        look up a callable
        """
        _class = self.__class__.__name__
        _test = self.dispatch_test
        _missing_error_message = (
            f"{_class} is missing the `{_test}` method. "
            f"Define `{_class}.{_test}` or override "
            f"`{_class}.get_dispatch_test."
        )
        _callable_error_message = f"{_class}.{_test} must be a callable."
        if self.dispatch_test is None:
            raise ImproperlyConfigured(_missing_error_message)

        try:
            method = getattr(self, self.dispatch_test)
        except AttributeError as exc:
            raise ImproperlyConfigured(_missing_error_message) from exc

        if not callable(method):
            raise ImproperlyConfigured(_callable_error_message)

        return method

    def handle_test_failure(self):
        """Test failed, raise an exception or redirect."""
        raise PermissionDenied


class SuperuserRequiredMixin(PassesTestMixin):
    """Require the user to be an authenticated superuser."""

    dispatch_test: str = "test_superuser"

    def test_superuser(self) -> bool:
        """The user must be both authenticated and a superuser."""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.is_superuser
        return False


class StaffUserRequiredMixin(PassesTestMixin):
    """Require the user to be an authenticated staff user."""

    dispatch_test: str = "test_staffuser"

    def test_staffuser(self) -> bool:
        """The user must be authenticated and `is_staff` must be True."""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.is_staff
        return False


class GroupRequiredMixin(PassesTestMixin):
    """Requires an authenticated user who is also a group member."""

    group_required: Union[str, list[str]] = None
    dispatch_test: str = "check_groups"

    def get_group_required(self) -> list[str]:
        """Return a list of required groups."""
        if self.group_required is None:
            _class = self.__class__.__name__
            _err_msg = (
                f"{_class} is missing the `group_required` "
                f"attribute. Define `{_class}.group_required` or"
                f"override `{_class}.get_group_required()."
            )
            raise ImproperlyConfigured(_err_msg)
        if isinstance(self.group_required, str):
            return [self.group_required]
        return self.group_required

    def check_membership(self) -> bool:
        """Check the user's membership in the required groups."""
        return bool(
            set(self.get_group_required()).intersection(
                [group.name for group in self.request.user.groups.all()]
            )
        )

    def check_groups(self) -> bool:
        """Check that the user is authenticated and a group member."""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and self.check_membership()
        return False


class AnonymousRequiredMixin(PassesTestMixin):
    """Require the user to be anonymous."""

    dispatch_test: str = "test_anonymous"
    redirect_unauthenticated_users: bool = False

    def test_anonymous(self) -> bool:
        """Accept anonymous users."""
        if (user := getattr(self.request, "user", None)) is not None:
            return not user.is_authenticated
        return True


class LoginRequiredMixin(PassesTestMixin):
    """Require the user to be authenticated."""

    dispatch_test: str = "test_authenticated"

    def test_authenticated(self) -> bool:
        """The user must be authenticated."""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated
        return False


class RecentLoginRequiredMixin(PassesTestMixin):
    """Require the user to be recently authenticated."""

    dispatch_test: str = "test_recent_login"
    max_age: int = 1800  # 30 minutes

    def test_recent_login(self) -> bool:
        """Make sure the user's login is recent enough."""
        if (user := getattr(self.request, "user", None)) is not None:
            return user.is_authenticated and user.last_login > now() - timedelta(
                seconds=self.max_age
            )
        return False

    def handle_test_failure(self) -> HttpResponseRedirect:
        """Logout the user and redirect to login."""
        return logout_then_login(self.request)


class PermissionRequiredMixin(PassesTestMixin):
    """Require a user to have specific permission(s)."""

    permission_required: Union[str, dict[str, list[str]]] = None
    dispatch_test: str = "check_permissions"

    def get_permission_required(self) -> Union[str, dict[str, list[str]]]:
        """Return a dict of required and optional permissions."""
        if self.permission_required is None:
            _class = self.__class__.__name__
            _err_msg = (
                f"{_class} is missing the `permission_required` attribute. "
                f"Define `{_class}.permission_required` or "
                f"override `{_class}.get_permission_required()`."
            )
            raise ImproperlyConfigured(_err_msg)
        if isinstance(self.permission_required, str):
            return {"all": [self.permission_required]}
        return self.permission_required

    def check_permissions(self) -> bool:
        """Check user for appropriate permissions."""
        permissions = self.get_permission_required()
        _all = permissions.get("all", [])
        _any = permissions.get("any", [])

        if not getattr(self.request, "user", None):
            return False
        perms_all = self.request.user.has_perms(_all) or []
        perms_any = [self.request.user.has_perm(perm) for perm in _any]

        return any((perms_all, any(perms_any)))


class SSLRequiredMixin(PassesTestMixin):
    """Require the request to be using SSL."""

    dispatch_test: str = "test_ssl"
    redirect_to_ssl: bool = True

    def test_ssl(self) -> bool:
        """Reject non-SSL requests."""
        # SSL isn't usually used/available during testing, so skip it.
        if getattr(settings, "DEBUG", False):
            return True  # pragma: no cover

        return self.request.is_secure()

    def handle_test_failure(
        self,
    ) -> Union[HttpResponse, BadRequest]:
        """Redirect to the SSL version of the request's URL."""
        if self.redirect_to_ssl:
            current = self.request.build_absolute_uri(self.request.get_full_path())
            secure = current.replace("http://", "https://")
            return HttpResponsePermanentRedirect(secure)
        raise BadRequest
