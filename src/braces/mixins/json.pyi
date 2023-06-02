from __future__ import annotations
from django.http import JsonResponse
from django.core.serializers import Serializer
from typing import *
from . import HasContext, A, K, HasContent

__all__ = ["JSONResponseMixin"]


class JSONResponseMixin(HasContent, HasContext):
    content_type: str
    context: K
    json_dumps_kwargs: K
    json_encoder_class: Type[Serializer]
    def get_content_type(self) -> str: ...
    def get_json_dumps_kwargs(self) -> K: ...
    def get_json_encoder_class(self) -> Type[Serializer]: ...
    def render_json_response(
        self, context: K = ..., status: int = ...
    ) -> JsonResponse: ...
