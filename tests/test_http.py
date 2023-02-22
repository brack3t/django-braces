import pytest

from django.http import HttpResponse
from django.test import RequestFactory
from django.views.generic import View

from braces import mixins


class TestAllVerbs:
    class _View(mixins.AllVerbsMixin, View):
        def all(self, request):
            return HttpResponse("OK")

    @pytest.mark.parametrize("verb", ["get", "post", "put", "patch", "delete", "head", "options", "trace"])
    def test_verbs(self, verb, rf):
        request = getattr(rf, verb)("/")
        response = self._View.as_view()(request)
        assert response.status_code == 200


class TestHeader:
    def test_headers(self, rf):
        class _View(mixins.HeaderMixin, View):
            headers = {"X-Test": "YES"}

            def get(self, request):
                return HttpResponse("OK")

        response = _View.as_view()(rf.get("/"))
        assert response["X-Test"] == "YES"

    def test_headers_unset(self, rf):
        class _View(mixins.HeaderMixin, View):
            def get(self, request):
                return HttpResponse("OK")

        response = _View.as_view()(rf.get("/"))

        with pytest.raises(KeyError):
            response["X-Test"]

    def test_existing_headers(self, rf):
        class _View(mixins.HeaderMixin, View):
            headers = {"X-Test": "YES"}

            def get(self, request):
                response = HttpResponse("OK")
                response.headers['Age'] = 120
                return response

        response = _View.as_view()(rf.get("/"))
        assert response["X-Test"] == "YES"
        assert response["Age"] == "120"


class TestNeverCache:
    class _View(mixins.NeverCacheMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def test_never_cache(self, rf):
        response = self._View.as_view()(rf.get("/"))
        assert response["Cache-Control"] == "max-age=0, no-cache, no-store, must-revalidate, private"
