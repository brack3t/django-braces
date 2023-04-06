import inspect
from django import http
from django.contrib.auth.views import redirect_to_login
from django.core.exceptions import ImproperlyConfigured


class RedirectMixin:
    """Mixin to simplify redirecting a request."""

    redirect_url: str = None

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return http.HttpResponseRedirect(self.get_redirect_url())

    def get_redirect_url(self) -> str:
        """Get the URL to redirect to"""
        if self.redirect_url is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a redirect_url. Define "
                f"{name}.redirect_url, or override "
                f"{name}.get_redirect_url()."
            )
        return self.redirect_url


class CanonicalRedirectMixin(RedirectMixin):
    """Redirect to the canonical URL for an object"""

    canonical_redirect: bool = False
    slug_field: str = "slug"
    slug_url_kwarg: str = "slug"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.canonical_redirect:
            self.redirect_url = self.get_canonical_url()

    def get_canonical_url(self) -> str:
        """Generate the canonical URL for the page"""
        raise NotImplementedError

    def dispatch(self, request, *args, **kwargs):
        """Check the slug and redirect if necessary"""
        slug_field = getattr(self.get_object(), self.slug_field, None)
        slug_kwarg = kwargs.get(self.slug_url_kwarg, None)

        if self.canonical_redirect and slug_field != slug_kwarg:
            return self.redirect(self.get_canonical_url())
        return super().dispatch(request, *args, **kwargs)

    def redirect(self, url=None) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return http.HttpResponseRedirect(url or self.get_redirect_url())


class RedirectOnFailureMixin(RedirectMixin):
    """Redirects to a login page if the request test fails"""

    redirect_url = "/"
    raise_exception: bool | Exception | Callable = False
    redirect_unauthenticated_users: bool = True

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

    def handle_test_failure(self) -> http.HttpResponse:
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
        """Generate a redirect"""
        return http.HttpResponseRedirect(self.get_redirect_url())


class RedirectToLoginMixin(RedirectOnFailureMixin):
    login_url: str = None
    redirect_field_name: str = REDIRECT_FIELD_NAME

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

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return redirect_to_login(
            self.request.get_full_path(),  # pylint: disable=no-member
            self.get_login_url(),
            self.get_redirect_field_name(),
        )


__all__ = [
    "RedirectMixin",
    "CanonicalRedirectMixin",
    "RedirectOnFailureMixin",
    "RedirectToLoginMixin",
]
