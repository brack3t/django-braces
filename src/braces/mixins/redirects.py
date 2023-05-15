"""Mixins that redirect requests."""

from __future__ import annotations

import inspect
import typing

from django import http
from django.conf import settings
from django.contrib.auth.views import redirect_to_login
from django.core.exceptions import ImproperlyConfigured

if typing.TYPE_CHECKING:
    from typing import Callable, Optional, Union

__all__ = [
    "RedirectMixin",
    "CanonicalRedirectMixin",
    "RedirectOnFailureMixin",
    "RedirectToLoginMixin",
]


class RedirectMixin:
    """Mixin to simplify redirecting a request."""

    redirect_url: Optional[str] = None

    def redirect(self: RedirectMixin) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL."""
        return http.HttpResponseRedirect(self.get_redirect_url())

    def get_redirect_url(self: RedirectMixin) -> str:
        """Get the URL to redirect to."""
        if self.redirect_url is None:
            _class = self.__class__.__name__
            _err_msg = (
                f"{_class} is missing the `redirect_url` attribute. "
                f"Define `{_class}.redirect_url` or override "
                f"`{_class}.get_redirect_url`."
            )
            raise ImproperlyConfigured(_err_msg)
        return self.redirect_url


class CanonicalRedirectMixin(RedirectMixin):
    """Redirect to the canonical URL for an object."""

    canonical_redirect: bool = False
    slug_field: str = "slug"
    slug_url_kwarg: str = "slug"

    def __init__(self: CanonicalRedirectMixin, *args, **kwargs) -> None:
        """Set `self.redirect_url` if needed."""
        super().__init__(*args, **kwargs)
        if self.canonical_redirect:
            self.redirect_url = self.get_canonical_url()

    def get_canonical_url(self: CanonicalRedirectMixin) -> str:
        """Generate the canonical URL for the page."""
        raise NotImplementedError

    def dispatch(
        self: CanonicalRedirectMixin, request: http.HttpRequest, *args, **kwargs
    ) -> http.HttpResponse:
        """Check the slug and redirect if necessary."""
        slug_field = getattr(self.get_object(), self.slug_field, None)
        slug_kwarg = kwargs.get(self.slug_url_kwarg, None)

        if self.canonical_redirect and slug_field != slug_kwarg:
            return self.redirect(self.get_canonical_url())
        return super().dispatch(request, *args, **kwargs)

    def redirect(
        self: CanonicalRedirectMixin, url: Optional[str] = None
    ) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL."""
        return http.HttpResponseRedirect(url or self.get_redirect_url())


class RedirectOnFailureMixin(RedirectMixin):
    """Redirect to `LOGIN_URL` if the request fails its tests."""

    redirect_url = "/"
    raise_exception: Union[bool, Exception, Callable] = False
    redirect_unauthenticated_users: bool = True

    def get_redirect_field_name(self: RedirectOnFailureMixin) -> str:
        """Return the query string field name for the redirection URL."""
        if self.redirect_field_name is None:
            _class = self.__class__.__name__
            _err_msg = (
                f"{_class} is missing the `redirect_field_name` attribute. "
                f"Define `{_class}.redirect_field_name` or override "
                f"`{_class}.get_redirect_field_name`."
            )
            raise ImproperlyConfigured(_err_msg)
        return self.redirect_field_name

    def handle_test_failure(self: RedirectOnFailureMixin) -> http.HttpResponse:
        """Handle a failed request with a redirect or an exception."""
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
        if callable(self.raise_exception) and isinstance(
            response := self.raise_exception(self.request),
            (http.HttpResponse, http.StreamingHttpResponse),
        ):
            return response

        # raise the default exception
        raise http.Http404

    def redirect(self: RedirectOnFailureMixin) -> http.HttpResponseRedirect:
        """Generate a redirect."""
        return http.HttpResponseRedirect(self.get_redirect_url())


class RedirectToLoginMixin(RedirectOnFailureMixin):
    """Redirect failed requests to `LOGIN_URL`."""

    login_url: Optional[str] = None
    redirect_field_name: Optional[str] = None

    def get_login_url(self: RedirectToLoginMixin) -> str:
        """Return the URL for the login page."""
        if self.login_url is None:
            try:
                self.login_url = settings.LOGIN_URL
            except AttributeError as exc:
                _class = self.__class__.__name__
                _err_msg = (
                    f"{_class} is missing the `login_url` attribute. "
                    f"Define `{_class}.login_url` or `settings.LOGIN_URL`."
                    f"Alternatively, override `{_class}.get_login_url`."
                )
                raise ImproperlyConfigured(_err_msg) from exc
        return self.login_url

    def redirect(self: RedirectToLoginMixin) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL."""
        return redirect_to_login(
            self.request.get_full_path(),
            self.get_login_url(),
            self.get_redirect_field_name(),
        )
