from unittest import mock

import pytest
from django.core.exceptions import ImproperlyConfigured
from django.views.generic.detail import SingleObjectMixin
from django.views.generic.list import MultipleObjectMixin

from braces import mixins
from .models import Article


class TestSelectRelated:
    class View(mixins.SelectRelatedMixin, SingleObjectMixin):
        model = Article
        select_related = ['foo', 'bar']

    def test_select_related(self):
        assert self.View().get_select_related() == ['foo', 'bar']

    def test_select_related_empty(self):
        view = self.View()
        view.select_related = None

        with pytest.raises(ImproperlyConfigured):
            view.get_select_related()

        view.select_related = ""
        with pytest.warns(UserWarning):
            view.get_select_related()

    def test_select_related_non_list(self):
        view = self.View()
        view.select_related = "foo"

        assert view.get_select_related() == ["foo"]

    @pytest.mark.django_db
    def test_select_related_existing_select_related(self):
        """New select_related should not cancel existing select_related"""
        view = self.View()
        view.select_related = "author"
        view.queryset = Article.objects.select_related('coauthor')
        assert view.get_queryset().query.select_related == {
            "author": {}, "coauthor": {}
        }


class TestPrefetchRelated:
    class View(mixins.PrefetchRelatedMixin, SingleObjectMixin):
        model = Article
        prefetch_related = ['foo', 'bar']

    def test_prefetch_related(self):
        assert self.View().get_prefetch_related() == ['foo', 'bar']

    def test_prefetch_related_string(self):
        view = self.View()
        view.prefetch_related = "foo"
        assert view.get_prefetch_related() == ["foo"]

    def test_prefetch_related_empty(self):
        view = self.View()
        view.prefetch_related = None

        with pytest.raises(ImproperlyConfigured):
            view.get_prefetch_related()

        view.prefetch_related = ""
        with pytest.warns(UserWarning):
            view.get_prefetch_related()

    @pytest.mark.django_db
    def test_prefetch_related_keeps_existing_prefetch_related(self):
        view = self.View()
        view.prefetch_related = "author"
        view.queryset = Article.objects.prefetch_related("coauthor")
        assert view.get_queryset()._prefetch_related_lookups == ("coauthor", "author")


class TestOrderableList:
    class View(mixins.OrderableListMixin, MultipleObjectMixin):
        model = Article
        orderable_columns = ("foo", "bar")

    def test_orderable_columns(self):
        assert self.View().get_orderable_columns() == ("foo", "bar")
