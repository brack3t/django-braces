import pytest
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django.views.generic import View


@pytest.fixture
def all_verbs_view(mixin_view):
    def view(**kwargs):
        out_class = type(
            "AllVerbsView", (mixin_view(), View), kwargs
        )
        out_class.all = lambda self, request: HttpResponse("OK")
        return out_class

    return view


@pytest.mark.mixin("AllVerbsMixin")
class TestAllVerbs:
    @pytest.mark.parametrize(
        "verb",
        ["get", "post", "put", "patch", "delete", "head", "options", "trace"],
    )
    def test_verbs(self, verb, all_verbs_view, rf):
        request = getattr(rf, verb)("/")
        response = all_verbs_view().as_view()(request)
        assert response.status_code == 200

    def test_undefined_handler(self, mixin_view, rf):
        with pytest.raises(NotImplementedError):
            mixin_view().as_view()(rf.get("/"))

    def test_missing_handler(self, all_verbs_view, rf):
        view = all_verbs_view()
        view.all_verb_handler = None
        with pytest.raises(ImproperlyConfigured):
            view.as_view()(rf.get("/"))


@pytest.fixture
def cache_view(mixin_view):
    def view(**kwargs):
        out_class = type(
            "CacheControlView", (mixin_view(), View), kwargs
        )
        out_class.cache_control_max_age = 120
        out_class.cache_control_no_cache = 120
        out_class.get = lambda self, request: HttpResponse("OK")
        return out_class

    return view

@pytest.mark.mixin("CacheControlMixin")
class TestCacheControl:
    def test_cache_control(self, cache_view, rf):
        response = cache_view().as_view()(rf.get("/"))
        assert "max-age=120" in response["Cache-Control"]
        assert "no-cache" in response["Cache-Control"]


@pytest.mark.mixin("HeaderMixin")
class TestHeader:
    def test_headers(self, mixin_view, rf):
        view = mixin_view(
            headers={"X-Test": "YES"},
        )
        response = view.as_view()(rf.get("/"))
        assert response["X-Test"] == "YES"

    def test_headers_unset(self, mixin_view, rf):
        view = mixin_view()
        response = view.as_view()(rf.get("/"))

        with pytest.raises(KeyError):
            response["X-Test"]

    def test_request_headers(self, mixin_view, rf):
        """Headers coming in on a request shouldn't come out on a response."""
        _resp = HttpResponse("OK", headers={"Age": 120})
        view = mixin_view(
            headers={"X-Test": "YES"},
            get=lambda s, r: _resp
        )
        response = view.as_view()(rf.get("/"))
        assert response["X-Test"] == "YES"
        with pytest.raises(KeyError):
            response["Age"]


@pytest.mark.mixin("NeverCacheMixin")
class TestNeverCache:
    def test_never_cache(self, mixin_view, rf):
        response = mixin_view().as_view()(rf.get("/"))
        assert (
            response["Cache-Control"]
            == "max-age=0, no-cache, no-store, must-revalidate, private"
        )
