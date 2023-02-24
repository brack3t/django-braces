import pytest
from django.core.exceptions import ImproperlyConfigured
from django.views.generic import TemplateView, View

from braces import mixins


class TestSetHeadline:
    def test_headline_attribute(self):
        class _View(mixins.SetHeadlineMixin, View):
            headline = "test"

        assert _View().get_headline() == "test"

    def test_headline_method(self):
        class _View(mixins.SetHeadlineMixin, View):
            def get_headline(self):
                return "test"

        assert _View().get_headline() == "test"

    def test_headline_missing(self):
        class _View(mixins.SetHeadlineMixin, View):
            pass

        with pytest.raises(ImproperlyConfigured):
            _View().get_headline()

    def test_headline_in_context(self):
        class _View(mixins.SetHeadlineMixin, TemplateView):
            headline = "test"

        assert _View().get_context_data()["headline"] == "test"


class TestStaticContext:
    def test_static_context_attribute(self):
        class _View(mixins.StaticContextMixin, View):
            static_context = {"test": "test"}

        assert _View().get_static_context() == {"test": "test"}

    def test_static_context_method(self):
        class _View(mixins.StaticContextMixin, View):
            def get_static_context(self):
                return {"test": "test"}

        assert _View().get_static_context() == {"test": "test"}

    def test_static_context_missing(self):
        class _View(mixins.StaticContextMixin, View):
            pass

        with pytest.raises(ImproperlyConfigured):
            _View().get_static_context()

    def test_static_context_in_context(self):
        class _View(mixins.StaticContextMixin, TemplateView):
            static_context = {"test": "test"}

        assert _View().get_context_data()["test"] == "test"
