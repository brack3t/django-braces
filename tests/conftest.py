"""Provides the `mixin_view` fixture."""

import logging
from importlib import import_module
from typing import Any, Callable, Dict

import pytest
from django.db.models import Model
from django.http import HttpResponse
from django.views.generic import View
from django.views.generic.detail import SingleObjectMixin
from django.views.generic.list import MultipleObjectMixin

from .project.models import Article

log = logging.getLogger(__name__)


@pytest.fixture()
@pytest.mark.django_db()
def user(django_user_model) -> Model:  # noqa: ANN001
    """Provide a generic user fixture for tests."""
    u = django_user_model.objects.create_user("test", "Test1234")
    yield u
    u.delete()


@pytest.fixture(name="mixin_view")
def mixin_view_factory(request: pytest.FixtureRequest) -> Callable:
    """Combine a mixin and View for a test."""
    mixin_request = request.node.get_closest_marker("mixin")
    if not mixin_request:
        pytest.fail("No mixin marker found")
    mixins = import_module(".mixins", "braces")
    mixin_name = mixin_request.args[0]
    mixin_class = getattr(mixins, mixin_name)

    def mixin_view(**kwargs: Dict[Any, Any]) -> type[View]:
        """Mixed-in view generator."""
        kwargs.update(
            {
                "get": lambda s, r: HttpResponse("django-braces"),
            }
        )
        return type("FixtureView", (mixin_class, View), kwargs)
    return mixin_view


@pytest.fixture()
def single_object_view(mixin_view):
    """Fixture for a view with the `SingleObjectMixin`."""

    def _view(**kwargs) -> type[SingleObjectMixin]:
        """Return a mixin view with the `SingleObjectMixin`."""
        return type(
            "SingleObjectView",
            (mixin_view(), SingleObjectMixin),
            {"model": Article}, **kwargs
        )
    return _view


@pytest.fixture()
def multiple_object_view(mixin_view):
    """Fixture for a view with the `MultipleObjectMixin`."""

    def _view(**kwargs) -> type[MultipleObjectMixin]:
        """Return a mixin view with the `MultipleObjectMixin`."""
        return type(
            "MultipleObjectView",
            (mixin_view(), MultipleObjectMixin),
            {"model": Article}, **kwargs
        )
    return _view
