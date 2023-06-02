from __future__ import annotations
from typing import *

A = Type[tuple[Any]]
K = Type[dict[Any, Any]]

class StaticContextMixin:
    static_context: dict[str, Any]
    def get_static_context(self) -> dict[str, Any]: ...
    def get_context_data(self, **kwargs: K) -> dict[str, Any]: ...
