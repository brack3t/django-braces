from __future__ import annotations
from typing import *
from django import forms
from django.http import HttpRequest, HttpResponse

from . import A, K

__all__ = [
    "MessageHelper",
    "MessageDescriptor",
    "MessagesMixin",
    "FormValidMessageMixin",
    "FormInvalidMessageMixin",
    "FormMessagesMixin",
]

class MessageHelper:
    API: Iterable[str]
    def __init__(self, request: HttpRequest) -> None: ...

class MessageDescriptor:
    def __get__(
        self, instance: HttpRequest, *args: A, **kwargs: K
    ) -> MessageHelper: ...
    def __set__(self, *args: A, **kwargs: K) -> AttributeError: ...
    def __delete__(self, *args: A, **kwargs: K) -> AttributeError: ...

class MessagesMixin:
    messages: MessageDescriptor

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
