"""Mixins related to Django REST Framework"""
from __future__ import annotations  # pylint: disable=unused-variable
from typing import Dict, Type

from django.core.exceptions import ImproperlyConfigured
from rest_framework.serializers import Serializer

# pylint: disable-next=unused-variable
__all__ = ["MultipleSerializersMixin"]


class MultipleSerializersMixin:
    """Mixin to use multiple serializers for a view.

    This mixin is useful if you want to use different serializers for
    different HTTP methods. For example, you may want to return a
    different set of fields for a GET request than for a POST request.
    """

    serializer_classes: Dict[str, Type[Serializer]] = None

    def get_serializer_classes(self) -> dict[str, Type[Serializer]]:
        """Return the classes used for serialization"""

        class_name = self.__class__.__name__
        if self.serializer_classes is None:
            raise ImproperlyConfigured(
                f"{class_name} is missing the serializer_classes attribute. "
                f"Define {class_name}.serializer_classes, or override "
                f"{class_name}.get_serializer_class()"
            )

        if not isinstance(self.serializer_classes, (dict, list, tuple)):
            raise ImproperlyConfigured(
                f"{class_name}.serializer_classes must be a "
                "dictionary or a series of two-tuples."
            )

        return self.serializer_classes

    def get_serializer_class(self) -> Type[Serializer]:
        """Return the class to use for the serializer.

        Defaults to using `super().serializer_class`.

        You may want to override this if you need to provide different
        serializations depending on the incoming request.

        (E.g. admins get full serialization, others get basic serialization)
        """

        serializer_classes = self.get_serializer_classes()
        return serializer_classes[self.request.method.lower()]
