"""Provides the `mixin_view` fixture."""
from typing import Callable

import pytest
from django.http import HttpResponse
from django.views.generic import View

from braces import mixins


@pytest.fixture(name="mixin_view")
def make_mixin_view(request) -> Callable:
    """Combine a mixin and View for a test."""

    mixin_request = request.node.get_closest_marker("mixin")
    if not mixin_request:
        pytest.fail("No mixin marker found")
    mixin_name = mixin_request.args[0]
    mixin_class = getattr(mixins, mixin_name, None)

    if mixin_class is None:
        pytest.fail(f"Couldn't find {mixin_name}")

    def _make_mixin_view(**kwargs) -> View:
        kwargs.update({
            "get": lambda s, r: HttpResponse("braces"),
        })
        view_class = type(
            "FixtureView",
            (mixin_class, View),
            kwargs
        )
        return view_class

    return _make_mixin_view
