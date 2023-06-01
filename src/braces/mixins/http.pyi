from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import *
    from typing import Any, Tuple, Type

    from django.http import HttpRequest, HttpResponse

A = Type[Tuple[Any]]
K = Type[Dict[Any, Any]]

class AllVerbsMixin:
    all_verb_handler: str
    def dispatch(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def all(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...

class HeaderMixin:
    headers: Dict[str, Any]
    def get_headers(self) -> Dict[str, Any]: ...
    def dispatch(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...

class CacheControlMixin:
    cache_control_public: bool
    cache_control_private: bool
    cache_control_no_cache: bool
    cache_control_no_store: bool
    cache_control_no_transform: bool
    cache_control_must_revalidate: bool
    cache_control_proxy_revalidate: bool
    cache_control_max_age: int
    cache_control_s_maxage: int
    @classmethod
    def get_cache_control_options(cls) -> Dict[Any, Any]: ...
    @classmethod
    def as_view(cls, **initkwargs: K) -> HttpResponse: ...

class NeverCacheMixin:
    @classmethod
    def as_view(cls, **initkwargs: K) -> HttpResponse: ...
