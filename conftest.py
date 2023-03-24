from importlib import import_module
import pytest
from django.http import HttpResponse
from django.views.generic import View


@pytest.fixture(scope="class")
def view_class(request):
    """Creates a view class with provided mixin class."""
    mixin_class = request.node.get_closest_marker("mixin_class")
    if mixin_class is None:
        pytest.fail("No `mixin class` specified for `view_class` fixture")
    else:
        mixin_class = import_module(
            f".mixins.{mixin_class.args[0]}",
            package="braces"
        )

    class FixtureView(mixin_class, View):
        def get(self, request):
            return HttpResponse("OK")

    return FixtureView.copy()
