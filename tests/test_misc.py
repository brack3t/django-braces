"""Tests related to the miscellaneous mixins."""

from typing import Type

import pytest
from django.core.exceptions import ImproperlyConfigured
from django.views.generic import TemplateView


@pytest.fixture()
def static_view(mixin_view):
    """Fixture for a TemplateView with the StaticContextMixin."""

    def _view(**kwargs) -> Type[TemplateView]:
        """Return a mixin view with the `StaticContextMixin`."""
        kwargs.update({"template_name": "test.html"})
        return type(
            "StaticView",
            (mixin_view(), TemplateView),
            kwargs,
        )

    return _view


@pytest.mark.mixin("StaticContextMixin")
class TestStaticContext:
    """Tests related to the `StaticContextMixin`."""

    def test_static_context_attribute(self, static_view):
        """Test that `static_context` is returned."""
        view = static_view(static_context={"django": "braces"})
        assert view().get_static_context() == {"django": "braces"}

    def test_static_context_method(self, static_view):
        """Test that `get_static_context` is returned."""
        view = static_view(get_static_context=lambda x: {"django": "braces"})
        assert view().get_static_context() == {"django": "braces"}

    def test_static_context_missing(self, static_view):
        """With no `static_context` attribute or method, raise an exception."""
        with pytest.raises(ImproperlyConfigured):
            static_view()().get_static_context()

    def test_static_context_in_context(self, static_view):
        """Test that `static_context` is in the view's context."""
        view = static_view(static_context={"django": "braces"})
        assert view().get_context_data()["django"] == "braces"
