import mock
from django import test
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from braces.views import AjaxResponseMixin
from .compat import force_text
from .factories import make_article, make_user
from .helpers import TestViewHelper
from .views import SimpleJsonView
from .compat import json


class TestAjaxResponseMixin(TestViewHelper, test.TestCase):
    """
    Tests for AjaxResponseMixin.
    """
    methods = ['get', 'post', 'put', 'delete']

    def test_xhr(self):
        """
        Checks if ajax_* method has been called for every http method.
        """
        # AjaxResponseView returns 'AJAX_OK' when requested with XmlHttpRequest
        for m in self.methods:
            fn = getattr(self.client, m)
            resp = fn('/ajax_response/',
                      HTTP_X_REQUESTED_WITH='XMLHttpRequest')
            assert force_text(resp.content) == 'AJAX_OK'

    def test_not_xhr(self):
        """
        Normal methods (get, post, etc) should be used when handling non-ajax
        requests.
        """
        for m in self.methods:
            fn = getattr(self.client, m)
            resp = fn('/ajax_response/')
            assert force_text(resp.content) == 'OK'

    def test_fallback_to_normal_methods(self):
        """
        Ajax methods should fallback to normal methods by default.
        """
        test_cases = [
            ('get', 'get'),
            ('post', 'post'),
            ('put', 'get'),
            ('delete', 'get'),
        ]

        for ajax_method, fallback in test_cases:
            m, mixin = mock.Mock(), AjaxResponseMixin()
            m.return_value = HttpResponse()
            req = self.build_request()
            setattr(mixin, fallback, m)
            fn = getattr(mixin, "%s_ajax" % ajax_method)
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
        self.assertEqual('application/json',
                         resp['content-type'].split(';')[0])

    def get_content(self, url):
        """
        GET url and return content
        """
        resp = self.client.get(url)
        self.assert_json_response(resp)
        content = force_text(resp.content)
        return content

    def test_simple_json(self):
        """
        Tests render_json_response() method.
        """
        user = make_user()
        self.client.login(username=user.username, password='asdf1234')
        data = json.loads(self.get_content('/simple_json/'))
        self.assertEqual({'username': user.username}, data)

    def test_serialization(self):
        """
        Tests render_json_object_response() method which serializes objects
        using django's serializer framework.
        """
        a1, a2 = [make_article() for __ in range(2)]
        data = json.loads(self.get_content('/article_list_json/'))
        self.assertIsInstance(data, list)
        self.assertEqual(2, len(data))
        titles = []
        for row in data:
            # only title has been serialized
            self.assertEqual(1, len(row['fields']))
            titles.append(row['fields']['title'])

        self.assertIn(a1.title, titles)
        self.assertIn(a2.title, titles)

    def test_missing_content_type(self):
        """
        ImproperlyConfigured exception should be raised if content_type
        attribute is not set correctly.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), content_type=None)

    def test_pretty_json(self):
        """
        Success if JSON responses are the same, and the well-indented response
        is longer than the normal one.
        """
        user = make_user()
        self.client.login(username=user.username, password='asfa')
        normal_content = self.get_content('/simple_json/')
        self.view_class.json_dumps_kwargs = {'indent': 2}
        pretty_content = self.get_content('/simple_json/')
        normal_json = json.loads(normal_content)
        pretty_json = json.loads(pretty_content)
        self.assertEqual(normal_json, pretty_json)
        self.assertTrue(len(pretty_content) > len(normal_content))
