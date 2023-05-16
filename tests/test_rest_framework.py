import pytest
from django.core.exceptions import ImproperlyConfigured
from rest_framework.generics import GenericAPIView

from braces import mixins


class TestMultipleSerializers:
    """Tests related to the `MultipleSerializersMixin`."""

    def test_get_serializer_class(self, rf):
        """Views are able to return a specific serializer class."""
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = {"get": "pass", "post": "fail"}

        request = rf.get("/")
        view = _View()
        view.setup(request)
        assert view.get_serializer_class() == "pass"

    def test_get_serializer_class_missing(self):
        """Views without `serializer_classes` raise an exception."""
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            pass

        with pytest.raises(ImproperlyConfigured):
            _View().get_serializer_class()

    def test_get_serializer_class_invalid(self):
        """Views with invalid `serializer_classes` raise an exception."""
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = "test"

        with pytest.raises(ImproperlyConfigured):
            _View().get_serializer_class()

    def test_get_serializer_class_invalid_method(self, rf):
        """Views without a serializer for the method raise an exception."""
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = {"post": "pass"}

        request = rf.get("/")
        view = _View()
        view.setup(request)

        with pytest.raises(KeyError):
            view.get_serializer_class()
