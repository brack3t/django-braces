from __future__ import annotations
from typing import Protocol, Any, Type
from .access import *
from .forms import *
from .http import *
from .json import *
from .messages import *
from .misc import *
from .queries import *
from .redirects import *
from .rest_framework import *

from django.http import HttpRequest, HttpResponse

A = Type[tuple[Any]]
K = Type[dict[str, Any]]
DOL = Type[str | dict[str, list[Any]]]  # String or Dict o' lists
RaiseOrCall = Type[bool | Exception | Callable[[], Callable[[], bool]]]

class CanDispatch(Protocol):
    """The concept of a view that can dispatch requests."""

    def dispatch(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...

class HasContext(Protocol):
    """The concept of `context`."""

    context: K

    def get_context_data(self) -> K: ...

class HasRequest(Protocol):
    """The existence of `self.request`."""

    request: HttpRequest

class HasContent(Protocol):
    """The concept of `content_type`."""

    content_type: str

    def get_content_type(self) -> str: ...

class HasHttpMethods(Protocol):
    def delete(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def get(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def head(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def options(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def patch(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def post(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def put(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
    def trace(self, request: HttpRequest, *args: A, **kwargs: K) -> HttpResponse: ...
