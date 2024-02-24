import json
from unittest import mock

from django import test
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django.utils.encoding import force_str

from braces.views import AjaxResponseMixin

from .factories import ArticleFactory, UserFactory
from .helpers import TestViewHelper
from .views import SimpleJsonView, JsonRequestResponseView, JsonBadRequestView


class TestAjaxResponseMixin(TestViewHelper, test.TestCase):
    """
    Tests for AjaxResponseMixin.
    """

    methods = ["get", "post", "put", "delete"]

    def test_xhr(self):
        """
        Checks if ajax_* method has been called for every http method.
        """
        # AjaxResponseView returns 'AJAX_OK' when requested with XmlHttpRequest
        for m in self.methods:
            fn = getattr(self.client, m)
            resp = fn(
                "/ajax_response/", HTTP_X_REQUESTED_WITH="XMLHttpRequest"
            )
            assert force_str(resp.content) == "AJAX_OK"

    def test_not_xhr(self):
        """
        Normal methods (get, post, etc) should be used when handling non-ajax
        requests.
        """
        for m in self.methods:
            fn = getattr(self.client, m)
            resp = fn("/ajax_response/")
            assert force_str(resp.content) == "OK"

    def test_fallback_to_normal_methods(self):
        """
        Ajax methods should fallback to normal methods by default.
        """
        test_cases = [
            ("get", "get"),
            ("post", "post"),
            ("put", "get"),
            ("delete", "get"),
        ]

        for ajax_method, fallback in test_cases:
            m, mixin = mock.Mock(), AjaxResponseMixin()
            m.return_value = HttpResponse()
            req = self.build_request()
            setattr(mixin, fallback, m)
            fn = getattr(mixin, "{0}_ajax".format(ajax_method))
            ret = fn(req, 1, 2, meth=ajax_method)
            # check if appropriate method has been called
            m.assert_called_once_with(req, 1, 2, meth=ajax_method)
            # check if appropriate value has been returned
            self.assertIs(m.return_value, ret)


class TestJSONResponseMixin(TestViewHelper, test.TestCase):
    """
    Tests for JSONResponseMixin.
    """

    view_class = SimpleJsonView

    def assert_json_response(self, resp, status_code=200):
        self.assertEqual(status_code, resp.status_code)
        self.assertEqual(
            "application/json", resp["content-type"].split(";")[0]
        )

    def get_content(self, url):
        """
        GET url and return content
        """
        resp = self.client.get(url)
        self.assert_json_response(resp)
        content = force_str(resp.content)
        return content

    def test_simple_json(self):
        """
        Tests render_json_response() method.
        """
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        data = json.loads(self.get_content("/simple_json/"))
        self.assertEqual({"username": user.username}, data)

    def test_serialization(self):
        """
        Tests render_json_object_response() method which serializes objects
        using django's serializer framework.
        """
        a1, a2 = [ArticleFactory() for __ in range(2)]
        data = json.loads(self.get_content("/article_list_json/"))
        self.assertIsInstance(data, list)
        self.assertEqual(2, len(data))
        titles = []
        for row in data:
            # only title has been serialized
            self.assertEqual(1, len(row["fields"]))
            titles.append(row["fields"]["title"])

        self.assertIn(a1.title, titles)
        self.assertIn(a2.title, titles)

    def test_bad_content_type(self):
        """
        ImproperlyConfigured exception should be raised if content_type
        attribute is not set correctly.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), content_type=["a"])

    def test_pretty_json(self):
        """
        Success if JSON responses are the same, and the well-indented response
        is longer than the normal one.
        """
        user = UserFactory()
        self.client.login(username=user.username, password="asfa")
        normal_content = self.get_content("/simple_json/")
        self.view_class.json_dumps_kwargs = {"indent": 2}
        pretty_content = self.get_content("/simple_json/")
        normal_json = json.loads("{0}".format(normal_content))
        pretty_json = json.loads("{0}".format(pretty_content))
        self.assertEqual(normal_json, pretty_json)
        self.assertTrue(len(pretty_content) > len(normal_content))

    def test_json_encoder_class_atrribute(self):
        """
        Tests setting custom `json_encoder_class` attribute.
        """
        data = json.loads(self.get_content("/simple_json_custom_encoder/"))
        self.assertEqual({"numbers": [1, 2, 3]}, data)


class TestJsonRequestResponseMixin(TestViewHelper, test.TestCase):
    view_class = JsonRequestResponseView
    request_dict = {"status": "operational"}

    def test_get_request_json_properly_formatted(self):
        """
        Properly formatted JSON requests should result in a JSON object
        """
        data = json.dumps(self.request_dict).encode("utf-8")
        response = self.client.post(
            "/json_request/", content_type="application/json", data=data
        )
        response_json = json.loads(response.content.decode("utf-8"))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response_json, self.request_dict)

    def test_get_request_json_improperly_formatted(self):
        """
        Improperly formatted JSON requests should make request_json == None
        """
        response = self.client.post("/json_request/", data=self.request_dict)
        response_json = json.loads(response.content.decode("utf-8"))
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response_json, None)

    def test_bad_request_response_with_custom_error_message(self):
        """
        If a view calls render_bad_request_response when request_json is empty
        or None, the client should get a 400 error
        """
        response = self.client.post(
            "/json_custom_bad_request/", data=self.request_dict
        )
        response_json = json.loads(response.content.decode("utf-8"))
        self.assertEqual(response.status_code, 400)
        self.assertEqual(response_json, {"error": "you messed up"})


class TestJsonBadRequestMixin(TestViewHelper, test.TestCase):
    view_class = JsonBadRequestView
    request_dict = {"status": "operational"}

    def test_bad_request_response(self):
        """
        If a view calls render_bad_request_response when request_json is empty
        or None, the client should get a 400 error
        """
        response = self.client.post(
            "/json_bad_request/", data=self.request_dict
        )
        response_json = json.loads(response.content.decode("utf-8"))
        self.assertEqual(response.status_code, 400)
        self.assertEqual(response_json, self.view_class.error_response_dict)

    def test_options_request_with_required_json_should_pass(self):
        """
        If a the client sends an OPTIONS request,
        even if require_json is set to true
        the client should get not a 400 error, because:
        * it's not possible to send HTTP body within an OPTIONS request
        * it's not up to an OPTIONS request to decide if the payload is valid
        """
        response = self.client.options("/json_bad_request/", data=None)
        self.assertEqual(response.status_code, 200)
