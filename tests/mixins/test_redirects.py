"""Tests relating to the redirection mixins."""

import pytest
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponseRedirect


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


@pytest.mark.mixin("CanonicalRedirectMixin")
class TestCanonicalURL:
    """Tests related to the `CanonicalRedirectMixin`."""

    def test_get_canonical_url(self, mixin_view):
        """`get_canonical_url` should return a string."""
        view = mixin_view()
        view.get_canonical_url = lambda x: "test"
        assert view().get_canonical_url() == "test"

    def test_get_canonical_url_missing(self, mixin_view):
        """Views without `get_canonical_url` raise an exception."""
        with pytest.raises(NotImplementedError):
            mixin_view()().get_canonical_url()

    def test_canonical_url(self, rf, mixin_view):
        """Views should not redirect canonical requests."""
        view = mixin_view(canonical_redirect=True)
        view.get_canonical_url = lambda x: "/canonical"
        request = rf.get("/canonical")
        response = view.as_view()(request)
        assert response.status_code == 200

    def test_non_canonical_url(self, rf, mixin_view):
        """Views should redirect non-canonical requests."""
        view = mixin_view(canonical_redirect=True)
        view.get_canonical_url = lambda x: "/canonical"
        request = rf.get("/non-canonical")
        response = view.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin("RedirectToLoginMixin")
class TestRedirectToLogin:
    """Tests related to the `RedirectToLoginMixin`."""

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
        """Views redirect requests."""
        view = mixin_view(redirect_url="/")
        request = rf.get("/secret/")
        response = view(request=request).redirect()
        assert response.status_code == 302
        assert isinstance(response, HttpResponseRedirect)

    def test_get_login_url(self, mixin_view):
        """A provided `login_url` is returned."""
        view = mixin_view(login_url="/login")
        assert view().get_login_url() == "/login"

    def test_get_login_url_exception(self, mixin_view, settings):
        """A provided `login_url` is returned."""
        del settings.LOGIN_URL
        view = mixin_view(login_url=None)
        with pytest.raises(ImproperlyConfigured):
            view().get_login_url()
