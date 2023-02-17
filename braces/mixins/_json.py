from typing import Iterable
from django.core import serializers
from django.core.exceptions import ImproperlyConfigured
from django.core.serializers.json import DjangoJSONEncoder
from django.http import JsonResponse, HttpResponse


class JSONResponseMixin:
    content_type: str = "application/json"
    json_dumps_kwargs: dict = None
    json_encoder_class: type = None

    def get_content_type(self) -> str:
        if self.content_type is None or not isinstance(self.content_type, str):
            class_name = self.__class__.__name__
            raise ImproperlyConfigured(
                f"{class_name} is missing a content type. Define {class_name}"
                f".content_type or override {class_name}.get_content_type()."
            )
        return self.content_type

    def get_json_dumps_kwargs(self) -> dict:
        dumps_kwargs = getattr(self, "json_dumps_kwargs", None) or {}
        dumps_kwargs.setdefault("ensure_ascii", False)
        return dumps_kwargs

    def get_json_encoder_class(self) -> type:
        if self.json_encoder_class is None:
            self.json_encoder_class = DjangoJSONEncoder
        return self.json_encoder_class

    def render_json_response(self, context: dict = None, status: int = 200):
        context = context or self.get_context() or {}
        return JsonResponse(
            data=context,
            safe=False,
            encoder=self.get_json_encoder_class(),
            json_dumps_params=self.get_json_dumps_kwargs(),
            content_type=self.get_content_type(),
            status=status,
        )

    def render_json_object_response(self, objects: Iterable, **kwargs):
        try:
            response = self.render_json_response(objects, **kwargs)
        except TypeError:
            json_data = serializers.serialize("json", objects, **kwargs)
            response = HttpResponse(json_data, content_type=self.get_content_type())
        return response


# Aliases for backwards compatibility
class JsonResponseMixin(JSONResponseMixin):
    ...