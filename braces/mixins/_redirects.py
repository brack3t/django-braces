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
