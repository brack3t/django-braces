from __future__ import annotations
from typing import Any, Protocol, TypeVar

from django.db import models
from django.forms import BaseForm, BaseModelForm
from django.http import HttpRequest, HttpResponse


_FormT = TypeVar("_FormT", bound=BaseForm)
_ModelFormT = TypeVar("_ModelFormT", bound=BaseModelForm)
_M = TypeVar("_M", bound=models.Model)


class BasicView(Protocol):
    def dispatch(self, request: HttpRequest):
        ...

    def get(self, request: HttpRequest, *args, **kwargs):
        ...

    def post(self, request: HttpRequest, *args, **kwargs):
        ...

    def put(self, request: HttpRequest, *args, **kwargs):
        ...

    def patch(self, request: HttpRequest, *args, **kwargs):
        ...

    def delete(self, request: HttpRequest, *args, **kwargs):
        ...

    def get_context_data(self, **kwargs):
        ...

    request: HttpRequest


class FormView(BasicView):
    initial: dict[str, Any]
    form_class: type[_FormT] | None
    success_url: str | None
    prefix: str | None

    def get_initial(self) -> dict[str, Any]:
        ...

    def get_form_class(self) -> type[_FormT]:
        ...

    def get_form_kwargs(self) -> dict[str, Any]:
        ...

    def form_valid(self, form: _FormT) -> HttpResponse:
        ...

    def form_invalid(self, form: _FormT) -> HttpResponse:
        ...

    def get_context_data(self, **kwargs: Any) -> dict[str, Any]:
        ...


class ModelFormView(BasicView):
    def get_form_class(self) -> type[_ModelFormT]:
        ...

    def get_form_kwargs(self) -> dict[str, Any]:
        ...

    def form_valid(self, form: _ModelFormT) -> HttpResponse:
        ...
