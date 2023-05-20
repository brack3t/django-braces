from django.http import JsonResponse
from django.core.serializers import Serializer
from typing import *

class HasContentType(Protocol):
    """The concept of `content_type`."""

    content_type: str

    def get_content_type(self) -> str: ...

class HasContext(Protocol):
    """The concept of `context`."""

    context: Dict[str, Any]

    def get_context_data(self) -> Dict[str, Any]: ...

class JSONResponseMixin(HasContentType, HasContext):
    content_type: str
    context: dict[str, Any]
    json_dumps_kwargs: Dict[str, Any]
    json_encoder_class: Type[Serializer]
    def get_content_type(self) -> str: ...
    def get_json_dumps_kwargs(self) -> Dict[str, Any]: ...
    def get_json_encoder_class(self) -> Type[Serializer]: ...
    def render_json_response(
        self, context: Dict[str, Any] = ..., status: int = ...
    ) -> JsonResponse: ...
