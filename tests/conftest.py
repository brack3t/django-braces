import copy
import pytest
from django.http import HttpResponse
from django.views.generic import View

from braces import mixins

MIXINS_TABLE = (
    ("PassesTest", mixins.PassesTest),
    ("RedirectOnFailure", mixins.RedirectOnFailure),
    ("SuperuserRequiredMixin", mixins.SuperuserRequiredMixin),
    ("StaffUserRequiredMixin", mixins.StaffUserRequiredMixin),
    ("GroupRequiredMixin", mixins.GroupRequiredMixin),
    ("AnonymousRequiredMixin", mixins.AnonymousRequiredMixin),
    ("LoginRequiredMixin", mixins.LoginRequiredMixin),
    ("RecentLoginRequiredMixin", mixins.RecentLoginRequiredMixin),
    ("PermissionRequiredMixin", mixins.PermissionRequiredMixin),
    ("SSLRequiredMixin", mixins.SSLRequiredMixin),
    ("UserFormMixin", mixins.UserFormMixin),
    ("FormWithUserMixin", mixins.FormWithUserMixin),
    ("CSRFExemptMixin", mixins.CSRFExemptMixin),
    ("MultipleFormsMixin", mixins.MultipleFormsMixin),
    ("MultipleModelFormsMixin", mixins.MultipleModelFormsMixin),
    ("AllVerbsMixin", mixins.AllVerbsMixin),
    ("HeaderMixin", mixins.HeaderMixin),
    ("CacheControlMixin", mixins.CacheControlMixin),
    ("NeverCacheMixin", mixins.NeverCacheMixin),
    ("JSONResponseMixin", mixins.JSONResponseMixin),
    ("MessagesMixin", mixins.MessagesMixin),
    ("FormValidMessageMixin", mixins.FormValidMessageMixin),
    ("FormInvalidMessageMixin", mixins.FormInvalidMessageMixin),
    ("FormMessagesMixin", mixins.FormMessagesMixin),
    ("StaticContextMixin", mixins.StaticContextMixin),
    ("SelectRelatedMixin", mixins.SelectRelatedMixin),
    ("PrefetchRelatedMixin", mixins.PrefetchRelatedMixin),
    ("OrderableListMixin", mixins.OrderableListMixin),
    ("RedirectMixin", mixins.RedirectMixin),
    ("CanonicalRedirectMixin", mixins.CanonicalRedirectMixin),
    ("MultipleSerializersMixin", mixins.MultipleSerializersMixin),
)


@pytest.fixture(name="mixin_view", scope="function")
def make_test_view(request) -> View:
    """Combine a mixin and View for a test."""
    mixin_class = request.node.get_closest_marker("mixin_class")
    if mixin_class and mixin_class is None:
        pytest.fail("No `mixin class` specified for `mixin_view` fixture")
    mixin_name = mixin_class.args[0]

    mixin = (cls for name, cls in MIXINS_TABLE if name == mixin_name)
    mixin = next(mixin, None)
    if mixin is None:
        pytest.fail(f"Couldn't find {mixin_name}")

    class FixtureView(mixin, View):
        """Temporary view with a specific mixin."""
        def get(self, request):
            """Reply to GETs."""
            return HttpResponse("OK")

    return copy.copy(FixtureView)
