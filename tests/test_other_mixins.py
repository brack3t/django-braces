import mock
from django import test
from django.core.exceptions import ImproperlyConfigured
from braces.views import SetHeadlineMixin
from .models import Article
from .helpers import TestViewHelper
from .views import CreateArticleView, ArticleListView
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
