from django.core.exceptions import ImproperlyConfigured
from django.views.decorators.cache import never_cache

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
    pass


class NeverCacheMixin:
    @classmethod
    def as_view(cls, **initkwargs):
        view = super().as_view(**initkwargs)
        return never_cache(view)
