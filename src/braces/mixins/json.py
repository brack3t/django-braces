"""Mixins related to JSON responses."""

from __future__ import annotations

from typing import TYPE_CHECKING

from django.core.exceptions import ImproperlyConfigured
from django.core.serializers.json import DjangoJSONEncoder
from django.http import HttpRequest, HttpResponse, JsonResponse

if TYPE_CHECKING:
    from typing import Any, Type


__all__ = [
    "JSONResponseMixin",
    "JSONRequestMixin",
    "JsonRequestMixin",
    "JsonResponseMixin",
]


class JSONResponseMixin:
    """A mixin that can be used to render a JSON response.

    NOTE: This is meant for light work. For heavy work, use a proper
    API framework.
    """

    content_type: str = "application/json"
    context: dict[str, Any] = {}
    json_dumps_kwargs: dict[str, Any] = {}
    json_encoder_class: Type = None

    def get_content_type(self) -> str:
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

    def get_json_dumps_kwargs(self) -> dict[str, Any]:
        """Collect kwargs for json.dumps()."""
        dumps_kwargs = getattr(self, "json_dumps_kwargs", None) or {}
        dumps_kwargs.setdefault("ensure_ascii", False)
        return dumps_kwargs

    def get_json_encoder_class(self) -> Type:
        """Get the appropriate JSON encoder."""
        if self.json_encoder_class is None:
            self.json_encoder_class = DjangoJSONEncoder
        return self.json_encoder_class

    def render_json_response(
        self,
        context: dict = None,
        status: int = 200,
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


class JSONRequestMixin:
    """A mixin to provide custom responses to JSON requests."""

    def dispatch(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Call the appropriate handler method."""
        if all(
            [
                request.headers.get("x-requested-with") == "XMLHttpRequest",
                request.method.lower() in self.http_method_names,
            ]
        ):
            handler = getattr(
                self,
                f"{request.method.lower()}_json",
                self.http_method_not_allowed,
            )
            self.request = request
            self.args = args
            self.kwargs = kwargs
            return handler(request, *args, **kwargs)

        return super().dispatch(request, *args, **kwargs)

    def get_json(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Handle a GET request."""
        return self.get(request, *args, **kwargs)

    def post_json(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Handle a POST request."""
        return self.post(request, *args, **kwargs)

    def patch_json(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Handle a PATCH request."""
        return self.patch(request, *args, **kwargs)

    def put_json(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Handle a PUT request."""
        return self.put(request, *args, **kwargs)

    def delete_json(self, request: HttpRequest, *args, **kwargs) -> HttpResponse:
        """Handle a DELETE request."""
        return self.delete(request, *args, **kwargs)


JsonRequestMixin = JSONRequestMixin
JsonResponseMixin = JSONResponseMixin
