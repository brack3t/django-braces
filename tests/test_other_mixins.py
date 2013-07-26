import mock
from django import test
from django.core.exceptions import ImproperlyConfigured
from braces.views import SetHeadlineMixin
from .models import Article
from .helpers import TestViewHelper
from .views import (CreateArticleView, ArticleListView, AuthorDetailView,
                    OrderableListView)
from .factories import make_user
from .compat import force_text


class TestCreateAndRedirectToEditView(TestViewHelper, test.TestCase):
    """
    Tests for CreateAndRedirectToEditView.
    """
    view_class = CreateArticleView

    def test_redirect(self):
        """
        Test if browser is redirected to edit view.
        """
        data = {'title': "Test body", 'body': "Test body"}
        resp = self.client.post('/article/create/', data)
        article = Article.objects.get()
        self.assertRedirects(resp, '/article/%d/edit/' % article.id)

    def test_missing_success_url_name(self):
        """
        Test if missing success_url_name causes ImproperlyConfigured error.
        """
        view = self.build_view(self.build_request(), success_url_name=None)
        with self.assertRaises(ImproperlyConfigured):
            view.get_success_url()


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
        user = make_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.post('/form_with_user_kwarg/', {'field1': 'foo'})
        assert force_text(resp.content) == "username: %s" % user.username

    def test_get_method(self):
        user = make_user()
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


class TestFormMessageMixins(test.TestCase):
    def test_it(self):
        resp = self.client.get('/form_messages/')

