from typing import Any, Dict, Generic, Type

from rest_framework.serializers import Serializer

class MultipleSerializersMixin:
    serializer_classes: Dict[str, Serializer[Any]]
    def get_serializer_classes(self) -> dict[str, Serializer[Any]]: ...
    def get_serializer_class(self) -> Serializer[Any]: ...
