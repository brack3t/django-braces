import pytest
from django.core.exceptions import ImproperlyConfigured
from rest_framework.generics import GenericAPIView

from braces import mixins


class TestMultipleSerializers:
    def test_get_serializer_class(self, rf):
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = {"get": "test"}

        request = rf.get("/")
        view = _View()
        view.setup(request)
        assert view.get_serializer_class() == "test"

    def test_get_serializer_class_missing(self):
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            pass

        with pytest.raises(ImproperlyConfigured):
            _View().get_serializer_class()

    def test_get_serializer_class_invalid(self):
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = "test"

        with pytest.raises(ImproperlyConfigured):
            _View().get_serializer_class()

    def test_get_serializer_class_invalid_method(self, rf):
        class _View(mixins.MultipleSerializersMixin, GenericAPIView):
            serializer_classes = {"post": "test"}

        request = rf.get("/")
        view = _View()
        view.setup(request)

        with pytest.raises(KeyError):
            view.get_serializer_class()
