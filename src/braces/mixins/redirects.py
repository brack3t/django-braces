"""Mixins that redirect requests."""

from __future__ import annotations

import inspect
from typing import TYPE_CHECKING

from django import http
from django.conf import settings
from django.contrib.auth.views import redirect_to_login
from django.core.exceptions import ImproperlyConfigured

if TYPE_CHECKING:
    from typing import Optional  # pragma: no cover

__all__ = [
    "RedirectMixin",
    "CanonicalRedirectMixin",
    "RedirectToLoginMixin",
]


class RedirectMixin:
    """Mixin to simplify redirecting a request."""

    redirect_url: str = ""

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL."""
        return http.HttpResponseRedirect(self.get_redirect_url())

    def get_redirect_url(self) -> str:
        """Get the URL to redirect to."""
        _url = getattr(self, "redirect_url", None)
        if _url is None or not _url:
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

    def __init__(self, *args, **kwargs) -> None:
        """Set `self.redirect_url` if needed."""
        super().__init__(*args, **kwargs)
        if self.canonical_redirect:
            self.redirect_url = self.get_canonical_url()

    def get_canonical_url(self) -> str:
        """Generate the canonical URL for the page.

        How could we possibly know what the canonical URL is?
        Your models, routes, and views are unique, so you'll
        have to implement this yourself.
        """
        raise NotImplementedError

    def dispatch(self, request: http.HttpRequest, *args, **kwargs) -> http.HttpResponse:
        """Check the slug and redirect if necessary."""
        if all([
            request.get_full_path() != self.get_redirect_url(),
            self.canonical_redirect,
        ]):
            return self.redirect()
        return super().dispatch(request, *args, **kwargs)


class RedirectToLoginMixin(RedirectMixin):
    """Redirect failed requests to `LOGIN_URL`."""

    login_url: Optional[str] = None

    def get_login_url(self) -> str:
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

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL."""
        return redirect_to_login(
            self.request.get_full_path(),
            self.get_login_url()
        )
