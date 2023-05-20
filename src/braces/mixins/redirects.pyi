from typing import *

from _typeshed import Incomplete
from django import http

A = Type[Tuple[Any]]
K = Type[Dict[Any, Any]]

class RedirectMixin:
    redirect_url: Optional[str]
    def redirect(self) -> http.HttpResponseRedirect: ...
    def get_redirect_url(self) -> str: ...

class CanonicalRedirectMixin(RedirectMixin):
    canonical_redirect: bool
    redirect_url: Incomplete
    def __init__(self, *args: A, **kwargs: K) -> None: ...
    def get_canonical_url(self) -> str: ...
    def dispatch(
        self, request: http.HttpRequest, *args: A, **kwargs: K
    ) -> http.HttpResponse: ...

class RedirectOnFailureMixin(RedirectMixin):
    redirect_url: str
    raise_exception: Union[bool, Exception, Callable[[Any], bool]]
    redirect_unauthenticated_users: bool
    def get_redirect_field_name(self) -> str: ...
    def handle_test_failure(self) -> Union[http.HttpResponse, Exception]: ...
    def redirect(self) -> http.HttpResponseRedirect: ...

class RedirectToLoginMixin(RedirectOnFailureMixin):
    login_url: Optional[str]
    redirect_field_name: Optional[str]
    def get_login_url(self) -> str: ...
    def redirect(self) -> http.HttpResponseRedirect: ...
