from unittest import mock
import pytest

from django.core.exceptions import ImproperlyConfigured
from django import test

from .helpers import TestViewHelper
from .models import Article
from .views import (
    ArticleListView,
    ArticleListViewWithCustomQueryset,
    AuthorDetailView,
    OrderableListView
)


class TestSelectRelatedMixin(TestViewHelper, test.TestCase):
    """Scenarios related to adding select_related to queries"""
    view_class = ArticleListView

    def test_missing_select_related(self):
        """If select_related is unset, raise ImproperlyConfigured"""
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), select_related=None)

    def test_invalid_select_related(self):
        """If select_related is not a list or tuple, raise ImproperlyConfigured"""
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), select_related={"a": 1})

    @mock.patch("django.db.models.query.QuerySet.select_related")
    def test_select_related_called(self, m):
        """QuerySet.select_related should be called with the correct arguments"""
        qs = Article.objects.all()
        m.return_value = qs.select_related("author")
        qs.select_related = m
        m.reset_mock()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(200, resp.status_code)
        m.assert_called_once_with("author")

    @mock.patch("django.db.models.query.QuerySet.select_related")
    def test_select_related_keeps_select_related_from_queryset(self, m):
        """
        Checks that an empty select_related attribute does not
        cancel a select_related provided by queryset.
        """
        qs = Article.objects.all()
        qs.select_related = m
        m.reset_mock()

        with pytest.warns(UserWarning):
            resp = self.dispatch_view(
                self.build_request(),
                view_class=ArticleListViewWithCustomQueryset,
            )
        self.assertEqual(200, resp.status_code)
        self.assertEqual(0, m.call_count)


class TestPrefetchRelatedMixin(TestViewHelper, test.TestCase):
    """Scenarios related to adding prefetch_related to queries"""
    view_class = AuthorDetailView

    def test_missing_prefetch_related(self):
        """If prefetch_related is missing/None, raise ImproperlyConfigured"""
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), prefetch_related=None)

    def test_invalid_prefetch_related(self):
        """If prefetch_related is not a list or tuple, raise ImproperlyConfigured"""
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), prefetch_related={"a": 1})

    @mock.patch("django.db.models.query.QuerySet.prefetch_related")
    def test_prefetch_related_called(self, m):
        """QuerySet.prefetch_related() should be called with correct arguments"""
        qs = Article.objects.all()
        m.return_value = qs.prefetch_related("article_set")
        qs.prefetch_related = m
        m.reset_mock()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(200, resp.status_code)
        m.assert_called_once_with("article_set")

    @mock.patch("django.db.models.query.QuerySet.prefetch_related")
    def test_prefetch_related_keeps_select_related_from_queryset(self, m):
        """
        Checks that an empty prefetch_related attribute does not
        cancel a prefetch_related provided by queryset.
        """
        qs = Article.objects.all()
        qs.prefetch_related = m
        m.reset_mock()

        with pytest.warns(UserWarning):
            resp = self.dispatch_view(
                self.build_request(),
                view_class=ArticleListViewWithCustomQueryset,
            )
        self.assertEqual(200, resp.status_code)
        self.assertEqual(0, m.call_count)


class TestOrderableListMixin(TestViewHelper, test.TestCase):
    """Scenarios involving ordering records"""
    view_class = OrderableListView

    def __make_test_articles(self):
        """Generate a couple of articles"""
        a1 = Article.objects.create(title="Alpha", body="Zet")
        a2 = Article.objects.create(title="Zet", body="Alpha")
        return a1, a2

    def test_correct_order(self):
        """Valid column and order query arguments should order the objects"""
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path="?order_by=title&ordering=asc"),
            orderable_columns=None,
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])

        resp = self.dispatch_view(
            self.build_request(path="?order_by=id&ordering=desc"),
            orderable_columns=None,
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a2, a1])

    def test_correct_order_with_default_ordering(self):
        """A valid order_by query argument should sort the default direction"""
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path="?order_by=id"),
            orderable_columns=None,
            ordering_default=None,
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])

        resp = self.dispatch_view(
            self.build_request(path="?order_by=id"),
            orderable_columns=None,
            ordering_default="asc",
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])

        resp = self.dispatch_view(
            self.build_request(path="?order_by=id"),
            orderable_columns=None,
            ordering_default="desc",
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a2, a1])

    def test_correct_order_with_param_not_default_ordering(self):
        """
        Objects must be properly ordered if requested with valid column names
        and ordering option in the query params.
        In this case, the ordering_default will be overwritten.
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path="?order_by=id&ordering=asc"),
            orderable_columns=None,
            ordering_default="desc",
            get_orderable_columns=lambda: (
                "id",
                "title",
            ),
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])

    def test_correct_order_with_incorrect_default_ordering(self):
        """
        Objects must be properly ordered if requested with valid column names
        and with the default ordering
        """
        view = self.view_class()
        view.ordering_default = "improper_default_value"
        self.assertRaises(
            ImproperlyConfigured, lambda: view.get_ordering_default()
        )

    def test_default_column(self):
        """
        When no ordering specified in GET, use
        View.get_orderable_columns_default()
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(self.build_request())
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])

    def test_get_orderable_columns_returns_correct_values(self):
        """
        OrderableListMixin.get_orderable_columns() should return
        View.orderable_columns attribute by default or raise
        ImproperlyConfigured exception if the attribute is None
        """
        view = self.view_class()
        self.assertEqual(view.get_orderable_columns(), view.orderable_columns)
        view.orderable_columns = None
        self.assertRaises(
            ImproperlyConfigured, lambda: view.get_orderable_columns()
        )

    def test_get_orderable_columns_default_returns_correct_values(self):
        """
        OrderableListMixin.get_orderable_columns_default() should return
        View.orderable_columns_default attribute by default or raise
        ImproperlyConfigured exception if the attribute is None
        """
        view = self.view_class()
        self.assertEqual(
            view.get_orderable_columns_default(),
            view.orderable_columns_default,
        )
        view.orderable_columns_default = None
        self.assertRaises(
            ImproperlyConfigured, lambda: view.get_orderable_columns_default()
        )

    def test_only_allowed_columns(self):
        """
        If column is not in Model.Orderable.columns iterable, the objects
        should be ordered by default column.
        """
        a1, a2 = self.__make_test_articles()

        resp = self.dispatch_view(
            self.build_request(path="?order_by=body&ordering=asc"),
            orderable_columns_default=None,
            get_orderable_columns_default=lambda: "title",
        )
        self.assertEqual(list(resp.context_data["object_list"]), [a1, a2])
