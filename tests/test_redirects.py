from unittest import mock

import pytest
from django.conf import settings
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django.test import RequestFactory
from django.views.generic import View

from braces import mixins


@pytest.fixture
def redirect_view():
    class _View(mixins.RedirectMixin, View):
        redirect_url = "/"
    yield _View


class Test_Redirect:
    def test_get_login_url(self, redirect_view):
        redirect_view.login_url = "test"
        assert redirect_view().get_login_url() == "test"

    def test_get_login_url_default(self, redirect_view):
        assert redirect_view().get_login_url() == settings.LOGIN_URL

    def test_get_login_url_missing(self, settings, redirect_view):
        del settings.LOGIN_URL
        redirect_view.login_url = None

        with pytest.raises(ImproperlyConfigured):
            redirect_view().get_login_url()

    def test_get_redirect_field_name(self, redirect_view):
        redirect_view.redirect_field_name = "test"
        assert redirect_view().get_redirect_field_name() == "test"

    def test_get_redirect_field_name_default(self, redirect_view):
        assert redirect_view().get_redirect_field_name() == REDIRECT_FIELD_NAME

    @mock.patch("braces.mixins.RedirectMixin.redirect")
    def test_test_failure(self, mock_redirect, redirect_view, rf):
        redirect_view.raise_exception = False
        redirect_view.request = rf.get("/")

        redirect_view().handle_test_failure()
        mock_redirect.assert_called()

    @mock.patch("braces.mixins.RedirectMixin.redirect")
    def test_test_anonymous(self, mock_redirect, redirect_view):
        redirect_view.raise_exception = False
        redirect_view.redirect_unauthenticated_users = True
        redirect_view.request = RequestFactory().get("/")
        redirect_view().handle_test_failure()
        assert mock_redirect.called

    @mock.patch("braces.mixins.RedirectMixin.redirect")
    def test_test_exception(self, mock_redirect, redirect_view):
        redirect_view.raise_exception = ImproperlyConfigured

        with pytest.raises(ImproperlyConfigured):
            redirect_view.as_view()(RequestFactory().get("/"))
        mock_redirect.assert_called_once()

    @mock.patch("braces.mixins.RedirectMixin.redirect")
    def test_test_callable(self, mock_redirect, redirect_view):
        redirect_view.test_method = lambda x: False

        with pytest.raises(ImproperlyConfigured):
            redirect_view.as_view()(RequestFactory().get("/"))
        mock_redirect.assert_called_once()


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
