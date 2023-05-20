from _typeshed import Incomplete
from typing import *
from django import forms
from django.http import HttpRequest, HttpResponse

A = Type[Tuple[Any]]
K = Type[Dict[Any, Any]]

class HasContext(Protocol):
    """The concept of `context`."""

    context: Dict[str, Any]

    def get_context_data(self) -> Dict[str, Any]: ...

class MessageHelper:
    API: Incomplete
    def __init__(self, request: HttpRequest) -> None: ...

class MessageDescriptor:
    def __get__(
        self, instance: HttpRequest, *args: A, **kwargs: K
    ) -> MessageHelper: ...
    def __set__(self, *args: A, **kwargs: K) -> AttributeError: ...
    def __delete__(self, *args: A, **kwargs: K) -> AttributeError: ...

class MessagesMixin:
    messages: Incomplete

class FormValidMessageMixin(MessagesMixin):
    form_valid_message: str
    def get_form_valid_message(self) -> str: ...
    def form_valid(self, form: forms.Form) -> HttpResponse: ...
    def delete(self, *args: A, **kwargs: K) -> HttpResponse: ...

class FormInvalidMessageMixin(MessagesMixin):
    form_invalid_message: str
    def get_form_invalid_message(self) -> str: ...
    def form_invalid(self, form: forms.Form) -> HttpResponse: ...

class FormMessagesMixin(FormValidMessageMixin, FormInvalidMessageMixin): ...
