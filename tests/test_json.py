from unittest import mock

import pytest
from django.views.generic import TemplateView, View


@pytest.fixture()
def json_response_view(mixin_view):
    def view(**kwargs):
        kwargs.update({"content_type": "application/yaml"})
        json_view = type("JSONResponseView", (mixin_view(), View), kwargs)
        json_view.get = lambda self, _: self.render_json_response({"foo": "bar"})
        return json_view

    return view


@pytest.fixture()
def json_request_view(mixin_view):
    def mock_json_get(self, request):
        return b'{"foo": "bar"}'

    def mock_get(self, request):
        return self.render_to_response({"foo": "bar"})

    def view(**kwargs):
        json_view = type("JSONRequestView", (mixin_view(), TemplateView), kwargs)
        json_view.template_name = "json.html"
        json_view.json_get = mock_json_get
        json_view.get = mock_get
        return json_view

    return view


@pytest.mark.mixin("JSONResponseMixin")
class TestJSONResponse:
    """Tests related to the `JSONResponseMixin`."""

    def test_content_type(self, json_response_view):
        """The content type shouldn't change."""
        view = json_response_view()
        assert view().get_content_type() == "application/yaml"

    def test_json_dumps_kwargs(self, json_response_view):
        """You can pass kwargs to json.dumps()."""
        view = json_response_view()
        view.json_dumps_kwargs = {"indent": 2}

        assert view().get_json_dumps_kwargs() == {
            "ensure_ascii": False,
            "indent": 2,
        }

    def test_json_encoder_class(self, json_response_view):
        """You can use a custom JSON encoder."""

        class Encoder:
            pass

        view = json_response_view()
        view.json_encoder_class = Encoder

        assert view().get_json_encoder_class() == Encoder

    def test_render_json_response(self, json_response_view):
        """The view will render a JSON response."""
        view = json_response_view()

        assert view().render_json_response({"foo": "bar"}).content == b'{"foo": "bar"}'


@pytest.mark.mixin("JSONRequestMixin")
class TestJSONRequestMixin:
    """Tests related to the `JSONRequestMixin`."""

    @mock.patch("braces.mixins.json.JSONRequestMixin.get_json")
    @pytest.mark.parametrize("json", [True, False])
    def test_json_get(self, mock_json_get, json, rf, json_request_view):
        """The view will render a JSON response."""
        view = json_request_view()
        headers = {}
        if json:
            headers = {
                "content_type": "application/json",
                "x-requested-with": "XMLHttpRequest",
            }

        req = rf.get(
            "/",
            headers=headers
        )
        view.as_view()(req)

        if json:
            assert mock_json_get.called
        else:
            assert mock_json_get.not_called
