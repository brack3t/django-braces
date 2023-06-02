"""Mixins that don't have a better home."""
from __future__ import annotations

from typing import Any

from django.core.exceptions import ImproperlyConfigured

__all__ = ["StaticContextMixin"]


class StaticContextMixin:
    """A mixin for adding static items to the context."""

    static_context: dict = None

    def get_static_context(self) -> dict:
        """Get the static context to add to the view's context."""
        _class = self.__class__.__name__
        if self.static_context is None:
            _err_msg = (
                f"{_class} is missing the static_context attribute. "
                f"Define `{_class}.static_context`, or override "
                f"`{_class}.get_static_context()`."
            )
            raise ImproperlyConfigured(_err_msg)

        if not isinstance(self.static_context, dict):
            _err_msg = f"{_class}.static_context must be a dictionary."
            raise ImproperlyConfigured(_err_msg)
        return self.static_context

    def get_context_data(self) -> dict[str, Any]:
        """Add the static context to the view's context."""
        context = super().get_context_data()
        context.update(self.get_static_context())
        return context
