from __future__ import annotations

from typing import *
from django.http import HttpRequest, HttpResponse

from . import A, K

class AllVerbsMixin:
    all_verb_handler: str
    def dispatch(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def all(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...

class HeaderMixin:
    headers: dict[str, Any]
    def get_headers(self) -> dict[str, Any]: ...
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
    def get_cache_control_options(cls) -> dict[Any, Any]: ...
    @classmethod
    def as_view(cls, **initkwargs: K) -> HttpResponse: ...

class NeverCacheMixin:
    @classmethod
    def as_view(cls, **initkwargs: K) -> HttpResponse: ...
