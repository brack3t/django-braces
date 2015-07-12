# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

import mock
import pytest

from django import VERSION as DJANGO_VERSION
from django.contrib import messages
from django.contrib.messages.middleware import MessageMiddleware
from django.contrib.messages.storage.base import Message
from django.core.exceptions import ImproperlyConfigured
from django.http import HttpResponse
from django import test
from django.test.utils import override_settings
from django.views.generic import View

from braces.views import (SetHeadlineMixin, MessageMixin, _MessageAPIWrapper,
                          FormValidMessageMixin, FormInvalidMessageMixin)
from .compat import force_text
from .factories import UserFactory
from .helpers import TestViewHelper
from .models import Article, CanonicalArticle
from .views import (ArticleListView, ArticleListViewWithCustomQueryset,
                    AuthorDetailView, OrderableListView,
                    FormMessagesView, ContextView)


class TestSuccessURLRedirectListMixin(test.TestCase):
    """
    Tests for SuccessURLRedirectListMixin.
    """
    def test_redirect(self):
        """
        Test if browser is redirected to list view.
        """
        data = {'title': "Test body", 'body': "Test body"}
        resp = self.client.post('/article_list/create/', data)
        self.assertRedirects(resp, '/article_list/')

    def test_no_url_name(self):
        """
        Test that ImproperlyConfigured is raised.
        """
        data = {'title': "Test body", 'body': "Test body"}
        with self.assertRaises(ImproperlyConfigured):
            self.client.post('/article_list_bad/create/', data)


class TestUserFormKwargsMixin(test.TestCase):
    """
    Tests for UserFormKwargsMixin.
    """
    def test_post_method(self):
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.post('/form_with_user_kwarg/', {'field1': 'foo'})
        assert force_text(resp.content) == "username: %s" % user.username

    def test_get_method(self):
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get('/form_with_user_kwarg/')
        assert resp.context['form'].user == user


class TestSetHeadlineMixin(test.TestCase):
    """
    Tests for SetHeadlineMixin.
    """
    def test_dynamic_headline(self):
        """
        Tests if get_headline() is called properly.
        """
        resp = self.client.get('/headline/test-headline/')
        self.assertEqual('test-headline', resp.context['headline'])

    def test_context_data(self):
        """
        Tests if mixin adds proper headline to template context.
        """
        resp = self.client.get('/headline/foo-bar/')
        self.assertEqual("foo-bar", resp.context['headline'])

    def test_get_headline(self):
        """
        Tests if get_headline() method works correctly.
        """
        mixin = SetHeadlineMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_headline()

        mixin.headline = "Test headline"
        self.assertEqual("Test headline", mixin.get_headline())

    def test_get_headline_lazy(self):
        resp = self.client.get('/headline/lazy/')
        self.assertEqual('Test Headline', resp.context['headline'])


class TestStaticContextMixin(test.TestCase):
    """ Tests for StaticContextMixin. """
    view_class = ContextView
    view_url = '/context/'

    def test_dict(self):
        self.view_class.static_context = {'test': True}
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(True, resp.context['test'])

    def test_two_tuple(self):
        self.view_class.static_context = [('a', 1), ('b', 2)]
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(1, resp.context['a'])
        self.assertEqual(2, resp.context['b'])

    def test_not_set(self):
        self.view_class.static_context = None
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_string_value_error(self):
        self.view_class.static_context = 'Fail'
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_list_error(self):
        self.view_class.static_context = ['fail', 'fail']
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)


class TestCsrfExemptMixin(test.TestCase):
    """
    Tests for TestCsrfExemptMixin.
    """
    def setUp(self):
        super(TestCsrfExemptMixin, self).setUp()
        self.client = self.client_class(enforce_csrf_checks=True)

    def test_csrf_token_is_not_required(self):
        """
        Tests if csrf token is not required.
        """
        resp = self.client.post('/csrf_exempt/', {'field1': 'test'})
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_text(resp.content))


class TestSelectRelatedMixin(TestViewHelper, test.TestCase):
    view_class = ArticleListView

    def test_missing_select_related(self):
        """
        ImproperlyConfigured exception should be raised if select_related
        attribute is missing.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), select_related=None)

    def test_invalid_select_related(self):
        """
        ImproperlyConfigured exception should be raised if select_related is
        not a tuple or a list.
        :return:
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), select_related={'a': 1})

    @mock.patch('django.db.models.query.QuerySet.select_related')
    def test_select_related_called(self, m):
        """
        Checks if QuerySet's select_related() was called with correct
        arguments.
        """
        qs = Article.objects.all()
        m.return_value = qs.select_related('author')
        qs.select_related = m
        m.reset_mock()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(200, resp.status_code)
        m.assert_called_once_with('author')

    @mock.patch('django.db.models.query.QuerySet.select_related')
    def test_select_related_keeps_select_related_from_queryset(self, m):
        """
        Checks that an empty select_related attribute does not
        cancel a select_related provided by queryset.
        """
        qs = Article.objects.all()
        qs.select_related = m
        m.reset_mock()

        resp = self.dispatch_view(
            self.build_request(),
            view_class=ArticleListViewWithCustomQueryset)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(0, m.call_count)


class TestPrefetchRelatedMixin(TestViewHelper, test.TestCase):
    view_class = AuthorDetailView

    def test_missing_prefetch_related(self):
        """
        ImproperlyConfigured exception should be raised if
        prefetch_related attribute is missing.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), prefetch_related=None)

    def test_invalid_prefetch_related(self):
        """
        ImproperlyConfigured exception should be raised if
        prefetch_related is not a tuple or a list.
        :return:
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), prefetch_related={'a': 1})

    @mock.patch('django.db.models.query.QuerySet.prefetch_related')
    def test_prefetch_related_called(self, m):
        """
        Checks if QuerySet's prefetch_related() was called with correct
        arguments.
        """
        qs = Article.objects.all()
        m.return_value = qs.prefetch_related('article_set')
        qs.prefetch_related = m
        m.reset_mock()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(200, resp.status_code)
        m.assert_called_once_with('article_set')

    @mock.patch('django.db.models.query.QuerySet.prefetch_related')
    def test_prefetch_related_keeps_select_related_from_queryset(self, m):
        """
        Checks that an empty prefetch_related attribute does not
        cancel a prefetch_related provided by queryset.
        """
        qs = Article.objects.all()
        qs.prefetch_related = m
        m.reset_mock()

        resp = self.dispatch_view(
            self.build_request(),
            view_class=ArticleListViewWithCustomQueryset)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(0, m.call_count)


class TestOrderableListMixin(TestViewHelper, test.TestCase):
    view_class = OrderableListView

    def __make_test_articles(self):
        a1 = Article.objects.create(title='Alpha', body='Zet')
        a2 = Article.objects.create(title='Zet', body='Alpha')
        return a1, a2

    def test_correct_order(self):
        """
        Objects must be properly ordered if requested with valid column names
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path='?order_by=title&ordering=asc'),
            orderable_columns=None,
            get_orderable_columns=lambda: ('id', 'title', ))
        self.assertEqual(list(resp.context_data['object_list']), [a1, a2])

        resp = self.dispatch_view(
            self.build_request(path='?order_by=id&ordering=desc'),
            orderable_columns=None,
            get_orderable_columns=lambda: ('id', 'title', ))
        self.assertEqual(list(resp.context_data['object_list']), [a2, a1])

    def test_default_column(self):
        """
        When no ordering specified in GET, use
        View.get_orderable_columns_default()
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(list(resp.context_data['object_list']), [a1, a2])

    def test_get_orderable_columns_returns_correct_values(self):
        """
        OrderableListMixin.get_orderable_columns() should return
        View.orderable_columns attribute by default or raise
        ImproperlyConfigured exception in the attribute is None
        """
        view = self.view_class()
        self.assertEqual(view.get_orderable_columns(), view.orderable_columns)
        view.orderable_columns = None
        self.assertRaises(ImproperlyConfigured,
                          lambda: view.get_orderable_columns())

    def test_get_orderable_columns_default_returns_correct_values(self):
        """
        OrderableListMixin.get_orderable_columns_default() should return
        View.orderable_columns_default attribute by default or raise
        ImproperlyConfigured exception in the attribute is None
        """
        view = self.view_class()
        self.assertEqual(view.get_orderable_columns_default(),
                         view.orderable_columns_default)
        view.orderable_columns_default = None
        self.assertRaises(ImproperlyConfigured,
                          lambda: view.get_orderable_columns_default())

    def test_only_allowed_columns(self):
        """
        If column is not in Model.Orderable.columns iterable, the objects
        should be ordered by default column.
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path='?order_by=body&ordering=asc'),
            orderable_columns_default=None,
            get_orderable_columns_default=lambda: 'title')
        self.assertEqual(list(resp.context_data['object_list']), [a1, a2])


class TestCanonicalSlugDetailView(test.TestCase):
    def setUp(self):
        Article.objects.create(title='Alpha', body='Zet', slug='alpha')
        Article.objects.create(title='Zet', body='Alpha', slug='zet')

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical.
        """
        resp = self.client.get('/article-canonical/1-alpha/')
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get('/article-canonical/2-zet/')
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get('/article-canonical/1-bad-slug/')
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get('/article-canonical/2-bad-slug/')
        self.assertEqual(resp.status_code, 301)


class TestNamespaceAwareCanonicalSlugDetailView(test.TestCase):
    def setUp(self):
        Article.objects.create(title='Alpha', body='Zet', slug='alpha')
        Article.objects.create(title='Zet', body='Alpha', slug='zet')

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical.
        """
        resp = self.client.get(
            '/article-canonical-namespaced/article/1-alpha/')
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get(
            '/article-canonical-namespaced/article/2-zet/')
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical and that the
        redirect is namespace aware.
        """
        resp = self.client.get(
            '/article-canonical-namespaced/article/1-bad-slug/')
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get(
            '/article-canonical-namespaced/article/2-bad-slug/')
        self.assertEqual(resp.status_code, 301)


class TestOverriddenCanonicalSlugDetailView(test.TestCase):
    def setUp(self):
        Article.objects.create(title='Alpha', body='Zet', slug='alpha')
        Article.objects.create(title='Zet', body='Alpha', slug='zet')

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical according to the
        overridden canonical slug.
        """
        resp = self.client.get('/article-canonical-override/1-nycun/')
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get('/article-canonical-override/2-mrg/')
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get('/article-canonical-override/1-bad-slug/')
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get('/article-canonical-override/2-bad-slug/')
        self.assertEqual(resp.status_code, 301)


class TestCustomUrlKwargsCanonicalSlugDetailView(test.TestCase):
    def setUp(self):
        Article.objects.create(title='Alpha', body='Zet', slug='alpha')
        Article.objects.create(title='Zet', body='Alpha', slug='zet')

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical
        """
        resp = self.client.get('/article-canonical-custom-kwargs/1-alpha/')
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get('/article-canonical-custom-kwargs/2-zet/')
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get('/article-canonical-custom-kwargs/1-bad-slug/')
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get('/article-canonical-custom-kwargs/2-bad-slug/')
        self.assertEqual(resp.status_code, 301)


class TestModelCanonicalSlugDetailView(test.TestCase):
    def setUp(self):
        CanonicalArticle.objects.create(
            title='Alpha', body='Zet', slug='alpha')
        CanonicalArticle.objects.create(
            title='Zet', body='Alpha', slug='zet')

    def test_canonical_slug(self):
        """
        Test that no redirect occurs when slug is canonical according to the
        model's canonical slug.
        """
        resp = self.client.get('/article-canonical-model/1-unauthored-alpha/')
        self.assertEqual(resp.status_code, 200)
        resp = self.client.get('/article-canonical-model/2-unauthored-zet/')
        self.assertEqual(resp.status_code, 200)

    def test_non_canonical_slug(self):
        """
        Test that a redirect occurs when the slug is non-canonical.
        """
        resp = self.client.get('/article-canonical-model/1-bad-slug/')
        self.assertEqual(resp.status_code, 301)
        resp = self.client.get('/article-canonical-model/2-bad-slug/')
        self.assertEqual(resp.status_code, 301)


# CookieStorage is used because it doesn't require middleware to be installed
@override_settings(
    MESSAGE_STORAGE='django.contrib.messages.storage.cookie.CookieStorage')
class MessageMixinTests(test.TestCase):
    def setUp(self):
        self.rf = test.RequestFactory()
        self.middleware = MessageMiddleware()

    def get_request(self, *args, **kwargs):
        request = self.rf.get('/')
        self.middleware.process_request(request)
        return request

    def get_response(self, request, view):
        response = view(request)
        self.middleware.process_response(request, response)
        return response

    def get_request_response(self, view, *args, **kwargs):
        request = self.get_request(*args, **kwargs)
        response = self.get_response(request, view)
        return request, response

    def test_add_messages(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.add_message(messages.SUCCESS, 'test')
                return HttpResponse('OK')

        request, response = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0].message, 'test')
        self.assertEqual(msg[0].level, messages.SUCCESS)

    def test_get_messages(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.add_message(messages.SUCCESS, 'success')
                self.messages.add_message(messages.WARNING, 'warning')
                content = ','.join(
                    m.message for m in self.messages.get_messages())
                return HttpResponse(content)

        _, response = self.get_request_response(TestView.as_view())
        self.assertEqual(response.content, b"success,warning")

    def test_get_level(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                return HttpResponse(self.messages.get_level())

        _, response = self.get_request_response(TestView.as_view())
        self.assertEqual(int(response.content), messages.INFO)  # default

    def test_set_level(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.set_level(messages.WARNING)
                self.messages.add_message(messages.SUCCESS, 'success')
                self.messages.add_message(messages.WARNING, 'warning')
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(msg, [Message(messages.WARNING, 'warning')])

    @override_settings(MESSAGE_LEVEL=messages.DEBUG)
    def test_debug(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.debug("test")
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.DEBUG, 'test'))

    def test_info(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.info("test")
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.INFO, 'test'))

    def test_success(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.success("test")
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.SUCCESS, 'test'))

    def test_warning(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.warning("test")
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.WARNING, 'test'))

    def test_error(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.error("test")
                return HttpResponse('OK')

        request, _ = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0], Message(messages.ERROR, 'test'))

    def test_invalid_attribute(self):
        class TestView(MessageMixin, View):
            def get(self, request):
                self.messages.invalid()
                return HttpResponse('OK')

        with self.assertRaises(AttributeError):
            self.get_request_response(TestView.as_view())

    @pytest.mark.skipif(
        DJANGO_VERSION < (1, 5),
        reason='Some features of MessageMixin are only available in '
               'Django >= 1.5')
    def test_wrapper_available_in_dispatch(self):
        """
        Make sure that self.messages is available in dispatch() even before
        calling the parent's implementation.
        """
        class TestView(MessageMixin, View):
            def dispatch(self, request):
                self.messages.add_message(messages.SUCCESS, 'test')
                return super(TestView, self).dispatch(request)

            def get(self, request):
                return HttpResponse('OK')

        request, response = self.get_request_response(TestView.as_view())
        msg = list(request._messages)
        self.assertEqual(len(msg), 1)
        self.assertEqual(msg[0].message, 'test')
        self.assertEqual(msg[0].level, messages.SUCCESS)

    def test_API(self):
        """
        Make sure that our assumptions about messages.api are still valid.
        """
        # This test is designed to break when django.contrib.messages.api
        # changes (items being added or removed).
        excluded_API = set()
        if DJANGO_VERSION >= (1, 7):
            excluded_API.add('MessageFailure')
        self.assertEqual(
            _MessageAPIWrapper.API | excluded_API,
            set(messages.api.__all__)
        )


class TestFormMessageMixins(test.TestCase):
    def setUp(self):
        self.good_data = {
            'title': 'Good',
            'body': 'Body'
        }
        self.bad_data = {
            'body': 'Missing title'
        }

    def test_valid_message(self):
        url = '/form_messages/'
        response = self.client.get(url)
        self.assertEqual(response.status_code, 200)

        response = self.client.post(url, self.good_data, follow=True)
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, FormMessagesView().form_valid_message)

    def test_invalid_message(self):
        url = '/form_messages/'
        response = self.client.get(url)
        self.assertEqual(response.status_code, 200)

        response = self.client.post(url, self.bad_data, follow=True)
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, FormMessagesView().form_invalid_message)

    def test_form_valid_message_not_set(self):
        mixin = FormValidMessageMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_valid_message()

    def test_form_valid_message_not_str(self):
        mixin = FormValidMessageMixin()
        mixin.form_valid_message = ['bad']
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_valid_message()

    def test_form_valid_returns_message(self):
        mixin = FormValidMessageMixin()
        mixin.form_valid_message = 'Good øø'
        self.assertEqual('Good øø', mixin.get_form_valid_message())

    def test_form_invalid_message_not_set(self):
        mixin = FormInvalidMessageMixin()
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_invalid_message()

    def test_form_invalid_message_not_str(self):
        mixin = FormInvalidMessageMixin()
        mixin.form_invalid_message = ['bad']
        with self.assertRaises(ImproperlyConfigured):
            mixin.get_form_invalid_message()

    def test_form_invalid_returns_message(self):
        mixin = FormInvalidMessageMixin()
        mixin.form_invalid_message = 'Bad øø'
        self.assertEqual('Bad øø', mixin.get_form_invalid_message())


class TestAllVerbsMixin(test.TestCase):
    def setUp(self):
        self.url = "/all_verbs/"
        self.no_handler_url = "/all_verbs_no_handler/"

    def test_options(self):
        response = self.client.options(self.url)
        self.assertEqual(response.status_code, 200)

    def test_get(self):
        response = self.client.get(self.url)
        self.assertEqual(response.status_code, 200)

    def test_head(self):
        response = self.client.head(self.url)
        self.assertEqual(response.status_code, 200)

    def test_post(self):
        response = self.client.post(self.url)
        self.assertEqual(response.status_code, 200)

    def test_put(self):
        response = self.client.put(self.url)
        self.assertEqual(response.status_code, 200)

    def test_delete(self):
        response = self.client.delete(self.url)
        self.assertEqual(response.status_code, 200)

    def test_no_all_handler(self):
        with self.assertRaises(ImproperlyConfigured):
            self.client.get('/all_verbs_no_handler/')
