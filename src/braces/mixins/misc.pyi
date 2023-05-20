from typing import Any, Dict, Tuple, Type

A = Type[Tuple[Any]]
K = Type[Dict[Any, Any]]

class StaticContextMixin:
    static_context: Dict[str, Any]
    def get_static_context(self) -> Dict[str, Any]: ...
    def get_context_data(self, **kwargs: K) -> Dict[str, Any]: ...
