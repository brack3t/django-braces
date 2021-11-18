from django.contrib import messages
from django.contrib.messages.middleware import MessageMiddleware
from django.contrib.messages.storage.base import Message
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django import test
from django.test.utils import override_settings
from django.views.generic import View
from django.utils.encoding import force_str

from braces.views import (
    SetHeadlineMixin,
    MessageMixin,
    FormValidMessageMixin,
    FormInvalidMessageMixin,
)
from .factories import UserFactory
from .models import Article, CanonicalArticle
from .views import (
    FormMessagesView,
    ContextView,
)


class TestSuccessURLRedirectListMixin(test.TestCase):
    """Scenarios around redirecting after a successful form submission"""

    def test_redirect(self):
        """Successful POST should redirect"""
        data = {"title": "Test body", "body": "Test body"}
        resp = self.client.post("/article_list/create/", data)
        self.assertRedirects(resp, "/article_list/")

    def test_no_url_name(self):
        """Improper setup should raise ImproperlyConfigured"""
        data = {"title": "Test body", "body": "Test body"}
        with self.assertRaises(ImproperlyConfigured):
            # The view at this endpoint has no success_url defined
            self.client.post("/article_list_bad/create/", data)


class TestUserFormKwargsMixin(test.TestCase):
    """Scenarios around automatically including a user in form submissions"""

    def test_post_method(self):
        """A POST request should include the user kwarg"""
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.post("/form_with_user_kwarg/", {"field1": "foo"})
        assert force_str(resp.content) == f"username: {user.username}"

    def test_get_method(self):
        """A GET request should include the user kwarg"""
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get("/form_with_user_kwarg/")
        assert resp.context["form"].user == user


class TestSetHeadlineMixin(test.TestCase):
    """Scenarios around setting a headline"""

    def test_dynamic_headline(self):
        """A method-provided headline should be included in context"""
        resp = self.client.get("/headline/test-headline/")
        self.assertEqual("test-headline", resp.context["headline"])

    def test_context_data(self):
        """An attribute-provided headline should be in the context"""
        resp = self.client.get("/headline/foo-bar/")
        self.assertEqual("foo-bar", resp.context["headline"])

    def test_improper_configuration(self):
        """Not providing a headline should raise an exception"""
        mixin = SetHeadlineMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_headline()

    def test_get_headline_lazy(self):
        """Lazy evaluation of text should still provide the headline"""
        resp = self.client.get("/headline/lazy/")
        self.assertEqual("Test Headline", resp.context["headline"])


class TestStaticContextMixin(test.TestCase):
    """Scenarios around including static content in the context"""

    view_class = ContextView
    view_url = "/context/"

    def test_dictionary(self):
        """Static content can be included as a dictionary"""
        self.view_class.static_context = {"test": True}
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(True, resp.context["test"])

    def test_two_tuple(self):
        """Static content can be included as a two-tuple pair"""
        self.view_class.static_context = [("a", 1), ("b", 2)]
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(1, resp.context["a"])
        self.assertEqual(2, resp.context["b"])

    def test_not_set(self):
        """ImproperlyConfigured should be raised if no static content is set"""
        self.view_class.static_context = None
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_string_value_error(self):
        """A string should raise ImproperlyConfigured"""
        self.view_class.static_context = "Fail"
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_list_error(self):
        """A list should raise ImproperlyConfigured"""
        self.view_class.static_context = ["fail", "fail"]
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)


class TestCsrfExemptMixin(test.TestCase):
    """Scenarios around views which are CSRF exempt"""

    def setUp(self):
        """Ensure the client enforces CSRF checks"""
        super(TestCsrfExemptMixin, self).setUp()
        self.client = self.client_class(enforce_csrf_checks=True)

    def test_csrf_token_is_not_required(self):
        """CSRF tokens should not be required"""
        resp = self.client.post("/csrf_exempt/", {"field1": "test"})
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))


class TestCanonicalSlugDetailView(test.TestCase):
    """Scenarios involving canonical slugs"""
    def setUp(self):
        """Create the two articles"""
        Article.objects.create(title="Alpha", body="Zet", slug="alpha")
        Article.objects.create(title="Zet", body="Alpha", slug="zet")

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical.
        """
        resp = self.client.get("/article-canonical/1-alpha/")
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get("/article-canonical/2-zet/")
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get("/article-canonical/1-bad-slug/")
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get("/article-canonical/2-bad-slug/")
        self.assertEqual(resp.status_code, 301)


class TestNamespaceAwareCanonicalSlugDetailView(test.TestCase):
    """Scenarios around canonical slugs and namespaces"""
    def setUp(self):
        """Create the necessary articles"""
        Article.objects.create(title="Alpha", body="Zet", slug="alpha")
        Article.objects.create(title="Zet", body="Alpha", slug="zet")

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical.
        """
        resp = self.client.get(
            "/article-canonical-namespaced/article/1-alpha/"
        )
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get("/article-canonical-namespaced/article/2-zet/")
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical and that the
        redirect is namespace aware.
        """
        resp = self.client.get(
            "/article-canonical-namespaced/article/1-bad-slug/"
        )
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get(
            "/article-canonical-namespaced/article/2-bad-slug/"
        )
        self.assertEqual(resp.status_code, 301)


class TestOverriddenCanonicalSlugDetailView(test.TestCase):
    """Scenarios involving overridden canonical slugs"""
    def setUp(self):
        """Create the necessary articles"""
        Article.objects.create(title="Alpha", body="Zet", slug="alpha")
        Article.objects.create(title="Zet", body="Alpha", slug="zet")

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical according to the
        overridden canonical slug.
        """
        resp = self.client.get("/article-canonical-override/1-nycun/")
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get("/article-canonical-override/2-mrg/")
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get("/article-canonical-override/1-bad-slug/")
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get("/article-canonical-override/2-bad-slug/")
        self.assertEqual(resp.status_code, 301)


class TestCustomUrlKwargsCanonicalSlugDetailView(test.TestCase):
    """Scenarios around canonical slugs and custom URL kwargs"""
    def setUp(self):
        """Create the articles"""
        Article.objects.create(title="Alpha", body="Zet", slug="alpha")
        Article.objects.create(title="Zet", body="Alpha", slug="zet")

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical
        """
        resp = self.client.get("/article-canonical-custom-kwargs/1-alpha/")
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get("/article-canonical-custom-kwargs/2-zet/")
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get("/article-canonical-custom-kwargs/1-bad-slug/")
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get("/article-canonical-custom-kwargs/2-bad-slug/")
        self.assertEqual(resp.status_code, 301)


class TestModelCanonicalSlugDetailView(test.TestCase):
    """Scenarios around canonical slugs and model fields"""
    def setUp(self):
        """Generate the necessary articles"""
        CanonicalArticle.objects.create(
            title="Alpha", body="Zet", slug="alpha"
        )
        CanonicalArticle.objects.create(title="Zet", body="Alpha", slug="zet")

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical according to the
        model's canonical slug.
        """
        resp = self.client.get("/article-canonical-model/1-unauthored-alpha/")
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get("/article-canonical-model/2-unauthored-zet/")
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get("/article-canonical-model/1-bad-slug/")
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get("/article-canonical-model/2-bad-slug/")
        self.assertEqual(resp.status_code, 301)


# CookieStorage is used because it doesn't require middleware to be installed
@override_settings(
    MESSAGE_STORAGE="django.contrib.messages.storage.cookie.CookieStorage"
)
class MessageMixinTests(test.TestCase):
    """Scenarios around the messaging framework"""
    def setUp(self):
        """Create necessary objects"""
        self.rf = test.RequestFactory()
        self.middleware = MessageMiddleware("")

    def get_request(self, *args, **kwargs):
        """Generate a request that has passed through the middleware"""
        request = self.rf.get("/")
        self.middleware.process_request(request)
        return request

    def get_response(self, request, view):
        """Generate a response that has been passed through the middleware"""
        response = view(request)
        self.middleware.process_response(request, response)
        return response

    def get_request_response(self, view, *args, **kwargs):
        """Get both a request and a response, middleware-processed"""
        request = self.get_request(*args, **kwargs)
        response = self.get_response(request, view)
        return request, response

    def test_add_messages(self):
        """Message should be added through the class attribute"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.add_message(messages.SUCCESS, "test")
                return HttpResponse("OK")

        request, response = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0].message, "test")
        self.assertEqual(msg[0].level, messages.SUCCESS)

    def test_get_messages(self):
        """get_messages should get the stored messages"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.add_message(messages.SUCCESS, "success")
                self.messages.add_message(messages.WARNING, "warning")
                content = ",".join(
                    m.message for m in self.messages.get_messages()
                )
                return HttpResponse(content)

        _, response = self.get_request_response(TestView.as_view())
        self.assertEqual(response.content, b"success,warning")

    def test_get_level(self):
        """Should be able to get message levels"""
        class TestView(MessageMixin, View):
            def get(self, request):
                return HttpResponse(self.messages.get_level())

        _, response = self.get_request_response(TestView.as_view())
        self.assertEqual(int(response.content), messages.INFO)  # default

    def test_set_level(self):
        """Should be able to set message levels"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.set_level(messages.WARNING)
                self.messages.add_message(messages.SUCCESS, "success")
                self.messages.add_message(messages.WARNING, "warning")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(msg, [Message(messages.WARNING, "warning")])

    @override_settings(MESSAGE_LEVEL=messages.DEBUG)
    def test_debug(self):
        """Messages should able to be set as DEBUG"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.debug("test")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.DEBUG, "test"))

    def test_info(self):
        """Messages should able to be set as INFO"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.info("test")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.INFO, "test"))

    def test_success(self):
        """Messages should able to be set as SUCCESS"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.success("test")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.SUCCESS, "test"))

    def test_warning(self):
        """Messages should able to be set as WARNING"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.warning("test")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.WARNING, "test"))

    def test_error(self):
        """Messages should able to be set as ERROR"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.error("test")
                return HttpResponse("OK")

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.ERROR, "test"))

    def test_invalid_attribute(self):
        """Raise an AttributeError if setting an invalid level"""
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.invalid()
                return HttpResponse("OK")

        with self.assertRaises(AttributeError):
            self.get_request_response(TestView.as_view())

    def test_wrapper_available_in_dispatch(self):
        """
        Make sure that self.messages is available in dispatch() even before
        calling the parent's implementation.
        """

        class TestView(MessageMixin, View):
            def dispatch(self, request):
                self.messages.add_message(messages.SUCCESS, "test")
                return super(TestView, self).dispatch(request)

            def get(self, request):
                return HttpResponse("OK")

        request, response = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0].message, "test")
        self.assertEqual(msg[0].level, messages.SUCCESS)

    def test_API(self):
        """
        Make sure that our assumptions about messages.api are still valid.
        """
        # This test is designed to break when django.contrib.messages.api
        # changes (items being added or removed).
        excluded_API = set()
        excluded_API.add("MessageFailure")


class TestFormMessageMixins(test.TestCase):
    """Scenarios around form valid/invalid messages"""
    def setUp(self):
        self.good_data = {"title": "Good", "body": "Body"}
        self.bad_data = {"body": "Missing title"}

    def test_valid_message(self):
        """If the form is valid, the valid message should be available"""
        url = "/form_messages/"
        response = self.client.get(url)
        self.assertEqual(response.status_code, 200)

        response = self.client.post(url, self.good_data, follow=True)
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, FormMessagesView().form_valid_message)

    def test_invalid_message(self):
        """If the form is invalid, the invalid message should be available"""
        url = "/form_messages/"
        response = self.client.get(url)
        self.assertEqual(response.status_code, 200)

        response = self.client.post(url, self.bad_data, follow=True)
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, FormMessagesView().form_invalid_message)

    def test_form_valid_message_not_set(self):
        """Not setting a form_valid message should raise ImproperlyConfigured"""
        mixin = FormValidMessageMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_valid_message()

    def test_form_valid_message_not_str(self):
        """Non-strings for the form_valid message should raise ImproperlyConfigured"""
        mixin = FormValidMessageMixin()
        mixin.form_valid_message = ["bad"]
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_valid_message()

    def test_form_valid_returns_message(self):
        """get_form_valid_message should return the form_valid message"""
        mixin = FormValidMessageMixin()
        mixin.form_valid_message = "Good øø"
        self.assertEqual(force_str("Good øø"), mixin.get_form_valid_message())

    def test_form_invalid_message_not_set(self):
        """Not setting a form_invalid message should raise ImproperlyConfigured"""
        mixin = FormInvalidMessageMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_invalid_message()

    def test_form_invalid_message_not_str(self):
        """Non-strings for the form_invalid message should raise ImproperlyConfigured"""
        mixin = FormInvalidMessageMixin()
        mixin.form_invalid_message = ["bad"]
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_invalid_message()

    def test_form_invalid_returns_message(self):
        """get_form_invalid_message should return the form_invalid message"""
        mixin = FormInvalidMessageMixin()
        mixin.form_invalid_message = "Bad øø"
        self.assertEqual(force_str("Bad øø"), mixin.get_form_invalid_message())


class TestAllVerbsMixin(test.TestCase):
    """Scenarios around the AllVerbsMixin"""
    def setUp(self):
        self.url = "/all_verbs/"
        self.no_handler_url = "/all_verbs_no_handler/"

    def test_options(self):
        """AllVerbs should respond to OPTION"""
        response = self.client.options(self.url)
        self.assertEqual(response.status_code, 200)

    def test_get(self):
        """AllVerbs should respond to GET"""
        response = self.client.get(self.url)
        self.assertEqual(response.status_code, 200)

    def test_head(self):
        """AllVerbs should respond to HEAD"""
        response = self.client.head(self.url)
        self.assertEqual(response.status_code, 200)

    def test_post(self):
        """AllVerbs should respond to POST"""
        response = self.client.post(self.url)
        self.assertEqual(response.status_code, 200)

    def test_put(self):
        """AllVerbs should respond to PUT"""
        response = self.client.put(self.url)
        self.assertEqual(response.status_code, 200)

    def test_delete(self):
        """AllVerbs should respond to DELETE"""
        response = self.client.delete(self.url)
        self.assertEqual(response.status_code, 200)

    def test_patch(self):
        """AllVerbs should respond to PATCH"""
        response = self.client.patch(self.url)
        self.assertEqual(response.status_code, 200)

    def test_no_all_handler(self):
        """A missing handler should raise ImproperlyConfigured"""
        with self.assertRaises(ImproperlyConfigured):
            self.client.get("/all_verbs_no_handler/")


class TestHeaderMixin(test.TestCase):
    """Scenarios around the extra headers mixin"""
    def test_attribute(self):
        """Headers can be set via an attribute"""
        response = self.client.get("/headers/attribute/")
        self.assertEqual(response["X-DJANGO-BRACES-1"], "1")
        self.assertEqual(response["X-DJANGO-BRACES-2"], "2")

    def test_method(self):
        """Headers can be set via a method"""
        response = self.client.get("/headers/method/")
        self.assertEqual(response["X-DJANGO-BRACES-1"], "1")
        self.assertEqual(response["X-DJANGO-BRACES-2"], "2")

    def test_existing(self):
        """Existing headers should still come through"""
        response = self.client.get('/headers/existing/')
        self.assertEqual(response['X-DJANGO-BRACES-EXISTING'], 'value')

class TestCacheControlMixin(test.TestCase):
    """Scenarios around controlling cache"""
    def test_cachecontrol_public(self):
        """Cache settings should be respected and included"""
        response = self.client.get('/cachecontrol/public/')
        options = [i.strip() for i in response['Cache-Control'].split(',')]
        self.assertEqual(sorted(options), ['max-age=60', 'public'])


class TestNeverCacheMixin(test.TestCase):
    """Scenarios around marking a view as never-cached"""
    def test_nevercache(self):
        """Views marked as no-cache should not be cached"""
        response = self.client.get('/nevercache/')
        options = [i.strip() for i in response['Cache-Control'].split(',')]
        expected_cache_control_options = {"max-age=0", "must-revalidate", "no-cache", "no-store", "private"}
        self.assertTrue(set(options).intersection(expected_cache_control_options))
