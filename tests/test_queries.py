from unittest import mock

import pytest
from django.core.exceptions import ImproperlyConfigured
from django.views.generic.detail import SingleObjectMixin

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