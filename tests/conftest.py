"""Provides the `mixin_view` fixture."""

import logging
from importlib import import_module
from typing import Any, Callable, Dict

import pytest
from django.http import HttpResponse
from django.views.generic import View

log = logging.getLogger(__name__)


@pytest.fixture(name="mixin_view")
def mixin_view_factory(request: pytest.FixtureRequest) -> Callable:
    """Combine a mixin and View for a test."""
    mixin_request = request.node.get_closest_marker("mixin")
    if not mixin_request:
        pytest.fail("No mixin marker found")
    mixins = import_module(".mixins", "braces")
    mixin_name = mixin_request.args[0]
    mixin_class = getattr(mixins, mixin_name)

    def mixin_view(**kwargs: Dict[Any, Any]) -> View:
        """Mixed-in view generator."""
        kwargs.update(
            {
                "get": lambda s, r: HttpResponse("django-braces"),
            }
        )
        return type("FixtureView", (mixin_class, View), kwargs)

    return mixin_view
