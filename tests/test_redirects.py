from unittest import mock

import pytest
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django.test import RequestFactory
from django.views.generic import View

from braces import mixins


@pytest.fixture()
@pytest.mark.mixin("RedirectMixin")
def redirect_view(mixin_view):
    """Create view that redirects to /."""

    mixin_view.redirect_url = "/"

    return mixin_view


@pytest.mark.mixin("RedirectMixin")
class TestRedirect:
    """Tests related to the `RedirectMixin`."""

    def test_get_redirect_url(self, mixin_view):
        """Views must have `redirect_view` defined."""
        view = mixin_view(redirect_url="/")
        assert view().get_redirect_url() == "/"

    def test_no_redirect_url(self, mixin_view):
        """An empty or missing `redirect_view` raises an exception."""
        view = mixin_view(redirect_url="")
        with pytest.raises(ImproperlyConfigured):
            view().get_redirect_url()

    def test_get_redirected(self, mixin_view, rf):
        """Views should redirect requests."""
        view = mixin_view(redirect_url="/")
        response = view().redirect()
        assert response.status_code == 302


class TestCanonicalURL:
    def test_get_canonical_url(self):
        class _View(mixins.CanonicalRedirectMixin, View):
            def get_canonical_url(self):
                return "test"

        assert _View().get_canonical_url() == "test"

    def test_get_canonical_url_missing(self):
        class _View(mixins.CanonicalRedirectMixin, View):
            pass

        with pytest.raises(NotImplementedError):
            _View().get_canonical_url()

    def test_dispatch(self, rf):
        class _View(mixins.CanonicalRedirectMixin, View):
            canonical_redirect = True
            slug_field = "slug"
            slug_url_kwarg = "slug"

            def get_canonical_url(self):
                return "test"

            def get_object(self):
                return mock.Mock(slug="test")

        response = _View().dispatch(rf.get("/"))
        assert response.url == "test"

    def test_non_canonical_url(self, rf):
        class _View(mixins.CanonicalRedirectMixin, View):
            canonical_redirect = True
            slug_field = "slug"
            slug_url_kwarg = "slug"

            def get_canonical_url(self):
                return "test"

            def get_object(self):
                return mock.Mock(slug="fail")

        redirect = _View().dispatch(rf.get("/?slug=fail"))
        assert redirect.url == "test"
