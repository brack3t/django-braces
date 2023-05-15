"""Mixins related to JSON responses."""

from __future__ import annotations

import typing

from django.core.exceptions import ImproperlyConfigured
from django.core.serializers.json import DjangoJSONEncoder
from django.http import JsonResponse

if typing.TYPE_CHECKING:
    from typing import Any, Type


__all__ = ["JSONResponseMixin"]


class JSONResponseMixin:
    """A mixin that can be used to render a JSON response.

    NOTE: This is meant for light work. For heavy work, use a proper
    API framework.
    """

    content_type: str = "application/json"
    json_dumps_kwargs: dict[str, Any] = None
    json_encoder_class: type = None

    def get_content_type(self: JSONResponseMixin) -> str:
        """Determine appropriate content type for response."""
        if self.content_type is None or not isinstance(self.content_type, str):
            _class = self.__class__.__name__
            _err_msg = (
                f"{_class} is missing the `content_type` attribute. "
                f"Define `{_class}.content_type` or override "
                f"`{_class}.get_content_type`."
            )
            raise ImproperlyConfigured(_err_msg)
        return self.content_type

    def get_json_dumps_kwargs(self: JSONResponseMixin) -> dict[str, Any]:
        """Collect kwargs for json.dumps()."""
        dumps_kwargs = getattr(self, "json_dumps_kwargs", None) or {}
        dumps_kwargs.setdefault("ensure_ascii", False)
        return dumps_kwargs

    def get_json_encoder_class(self: JSONResponseMixin) -> Type:
        """Get the appropriate JSON encoder."""
        if self.json_encoder_class is None:
            self.json_encoder_class = DjangoJSONEncoder
        return self.json_encoder_class

    def render_json_response(
        self: JSONResponseMixin, context: dict = None, status: int = 200
    ) -> JsonResponse:
        """Render a JSON response."""
        context = context or self.get_context_data() or {}
        return JsonResponse(
            data=context,
            safe=False,
            encoder=self.get_json_encoder_class(),
            json_dumps_params=self.get_json_dumps_kwargs(),
            content_type=self.get_content_type(),
            status=status,
        )
