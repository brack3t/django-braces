from django.core.exceptions import ImproperlyConfigured
from django.views.decorators.cache import cache_control, never_cache

class AllVerbsMixin:
    all_verb_handler: str = "all"

    def dispatch(self, request, *args, **kwargs):
        if not self.all_verb_handler:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} requires the all_verb_handler attribute to be set."
            )

        handler = getattr(self, self.all_verb_handler, self.http_method_not_allowed)
        return handler(request, *args, **kwargs)

    def all(self, request, *args, **kwargs):
        raise NotImplementedError


class HeaderMixin:
    headers: dict = None

    def get_headers(self, request) -> dict:
        if self.headers is None:
            self.headers = {}
        return self.headers

    def dispatch(self, request, *args, **kwargs):
        response = super().dispatch(request, *args, **kwargs)
        for key, value in self.get_headers(request).items():
            response[key] = value
        return response


class CacheControlMixin:
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
        options = {}
        for key, value in cls.__dict__.items():
            if key.startswith("cache_control_") and value is not None:
                options[key.replace("cache_control_", "")] = value
        return options

    @classmethod
    def as_view(cls, **initkwargs):
        view = super().as_view(**initkwargs)
        return cache_control(**cls.get_cache_control_options())(view)



class NeverCacheMixin:
    @classmethod
    def as_view(cls, **initkwargs):
        view = super().as_view(**initkwargs)
        return never_cache(view)
