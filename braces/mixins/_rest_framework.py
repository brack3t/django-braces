from typing import Dict, Type

from django.core.exceptions import ImproperlyConfigured
from rest_framework.serializers import Serializer


class MultipleSerializersMixin:
    """Mixin to use multiple serializers for a view.

    This mixin is useful if you want to use different serializers for
    different HTTP methods. For example, you may want to return a
    different set of fields for a GET request than for a POST request.
    """

    serializer_classes: Dict[str, Type[Serializer]] = None

    def get_serializer_class(self):
        """Return the class to use for the serializer.

        Defaults to using `super().serializer_class`.

        You may want to override this if you need to provide different
        serializations depending on the incoming request.

        (E.g. admins get full serialization, others get basic serialization)
        """

        class_name = self.__class__.__name__
        if self.serializer_classes is None:
            raise ImproperlyConfigured(
                f"{class_name} is missing the serializer_classes attribute. Define "
                f"{class_name}.serializer_classes, or override {class_name}.get_serializer_class()"
            )

        if not isinstance(self.serializer_classes, (dict, list, tuple)):
            raise ImproperlyConfigured(
                f"{class_name}.serializer_classes must be a "
                "dictionary or a series of two-tuples."
            )

        return self.serializer_classes[self.request.method.lower()]
