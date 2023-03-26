"""Mixins related to HTTP requests and responses"""
from __future__ import annotations  # pylint: disable=unused-variable

from django.core.exceptions import ImproperlyConfigured
from django.views.decorators.cache import cache_control, never_cache

# pylint: disable-next=unused-variable
__all__ = ["AllVerbsMixin", "HeaderMixin", "CacheControlMixin", "NeverCacheMixin"]


class AllVerbsMixin:
    """Handle all HTTP verbs with a single method."""

    all_verb_handler: str = "all"

    def dispatch(self, request, *args, **kwargs):
        """Run all requests through the all_verb_handler method."""
        if not self.all_verb_handler:
            err = (f"{self.__class__.__name__} requires the all_verb_handler "
                    "attribute to be set.")
            raise ImproperlyConfigured(err)

        handler = getattr(self, self.all_verb_handler, self.http_method_not_allowed)
        return handler(request, *args, **kwargs)

    def all(self, request, *args, **kwargs):
        """Handle all requests."""
        raise NotImplementedError


class HeaderMixin:
    """Mixin for easily adding headers to a response."""

    headers: dict = None

    def get_headers(self) -> dict:
        """Return a dictionary of headers to add to the response."""
        if self.headers is None:
            self.headers = {}
        return self.headers

    # pylint: disable-next=too-many-function-args
    def dispatch(self, request, *args, **kwargs):
        """Add headers to the response."""
        response = super().dispatch(request, *args, **kwargs)
        for key, value in self.get_headers().items():
            response[key] = value
        return response


class CacheControlMixin:
    """Provides a view with cache control options."""

    cache_control_public: bool = None
    cache_control_private: bool = None
    cache_control_no_cache: bool = None
    cache_control_no_store: bool = None
    cache_control_no_transform: bool = None
    cache_control_must_revalidate: bool = None
    cache_control_proxy_revalidate: bool = None
    cache_control_max_age: int = None
    cache_control_s_maxage: int = None

    @classmethod
    def get_cache_control_options(cls) -> dict:
        """What cache control options are set on the view?"""
        options = {}
        for key, value in cls.__dict__.items():
            if key.startswith("cache_control_") and value is not None:
                options[key.replace("cache_control_", "")] = value
        return options

    @classmethod
    def as_view(cls, **initkwargs):
        """Add cache control to the view."""
        view = super().as_view(**initkwargs)
        return cache_control(**cls.get_cache_control_options())(view)


class NeverCacheMixin:  # pylint: disable=too-few-public-methods
    """Prevents a view from being cached."""

    @classmethod
    def as_view(cls, **initkwargs):
        """Wrap the view with never_cache."""
        view = super().as_view(**initkwargs)
        return never_cache(view)
