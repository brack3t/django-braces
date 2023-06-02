from __future__ import annotations
from typing import *

from django import http

from . import A, K

class RedirectMixin:
    redirect_url: Optional[str]
    request: http.HttpRequest
    def redirect(self) -> http.HttpResponseRedirect: ...
    def get_redirect_url(self) -> str: ...

class CanonicalRedirectMixin(RedirectMixin):
    canonical_redirect: bool
    redirect_url: Optional[str]
    request: http.HttpRequest
    def __init__(self, *args: A, **kwargs: K) -> None: ...
    def get_canonical_url(self) -> str: ...
    def dispatch(
        self, request: http.HttpRequest, *args: A, **kwargs: K
    ) -> http.HttpResponse: ...

class RedirectToLoginMixin(RedirectMixin):
    login_url: Optional[str]
    request: http.HttpRequest
    def get_login_url(self) -> str: ...
    def redirect(self) -> http.HttpResponseRedirect: ...
