from __future__ import annotations
from django.http import JsonResponse
from django.core.serializers import Serializer
from typing import *
from . import HasContext

A = Type[tuple[Any]]
K = Type[dict[Any, Any]]

__all__ = ["JSONResponseMixin"]

class HasContent(Protocol):
    """The concept of `content_type`."""

    content_type: str

    def get_content_type(self) -> str: ...

class JSONResponseMixin(HasContent, HasContext):
    content_type: str
    context: dict[str, Any]
    json_dumps_kwargs: dict[str, Any]
    json_encoder_class: Type[Serializer]
    def get_content_type(self) -> str: ...
    def get_json_dumps_kwargs(self) -> dict[str, Any]: ...
    def get_json_encoder_class(self) -> Type[Serializer]: ...
    def render_json_response(
        self, context: dict[str, Any] = ..., status: int = ...
    ) -> JsonResponse: ...
