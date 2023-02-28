from django import http
from django.views.generic import View

from braces import mixins

from .project.models import Article


class TestJSONResponse:
    class _View(mixins.JSONResponseMixin, View):
        def get(self, request):
            return self.render_json_response({"foo": "bar"})

    def test_content_type(self):
        view = self._View()
        view.content_type = "application/yaml"

        assert view.get_content_type() == "application/yaml"

    def test_json_dumps_kwargs(self):
        view = self._View()
        view.json_dumps_kwargs = {"indent": 2}

        assert view.get_json_dumps_kwargs() == {
            "ensure_ascii": False,
            "indent": 2,
        }

    def test_json_encoder_class(self):
        class Encoder:
            pass

        view = self._View()
        view.json_encoder_class = Encoder

        assert view.get_json_encoder_class() == Encoder

    def test_render_json_response(self):
        view = self._View()

        request = http.HttpRequest()
        view.setup(request)

        assert (
            view.render_json_response({"foo": "bar"}).content
            == b'{"foo": "bar"}'
        )
