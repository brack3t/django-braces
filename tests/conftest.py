import copy
import pytest
from django.http import HttpResponse
from django.views.generic import View

from braces import mixins

MIXINS = (
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


@pytest.fixture(name="mixin_view")
def make_mixin_view() -> View:
    """Combine a mixin and View for a test."""

    def _make_mixin_view(mixin_name, kwargs=None):
        mixin = (cls for name, cls in MIXINS if name == mixin_name)
        mixin = next(mixin, None)
        if mixin is None:
            pytest.fail(f"Couldn't find {mixin_name}")

        if not kwargs or not isinstance(kwargs, dict):
            kwargs = {}
        kwargs.update({
            "get": lambda s, r: HttpResponse("braces"),
        })
        view_class = type(
            "FixtureView",
            (mixin, View),
            kwargs
        )
        return view_class

    return _make_mixin_view
