from django import http
from django.core.exceptions import ImproperlyConfigured


class RedirectMixin:
    redirect_url: str = None

    def redirect(self) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return http.HttpResponseRedirect(self.get_redirect_url())

    def get_redirect_url(self) -> str:
        if self.redirect_url is None:
            name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{name} is missing a redirect_url. Define "
                f"{name}.redirect_url, or override "
                f"{name}.get_redirect_url()."
            )
        return self.redirect_url


class CanonicalRedirectMixin(RedirectMixin):
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
        slug_field = getattr(self.get_object(), self.slug_field, None)
        slug_kwarg = kwargs.get(self.slug_url_kwarg, None)

        if self.canonical_redirect and slug_field != slug_kwarg:
            return self.redirect(self.get_canonical_url())
        return super().dispatch(request, *args, **kwargs)

    def redirect(self, url=None) -> http.HttpResponseRedirect:
        """Generate a redirect for the login URL"""
        return http.HttpResponseRedirect(url or self.get_redirect_url())
