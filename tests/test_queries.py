import pytest
from django.core.exceptions import ImproperlyConfigured
from django.views.generic.detail import SingleObjectMixin
from django.views.generic.list import MultipleObjectMixin

from braces import mixins
from .project.models import Article


class TestSelectRelated:
    class View(mixins.SelectRelatedMixin, SingleObjectMixin):
        model = Article
        select_related = ["foo", "bar"]

    def test_select_related(self):
        assert self.View().get_select_related() == ["foo", "bar"]

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
        view.queryset = Article.objects.select_related("coauthor")
        assert view.get_queryset().query.select_related == {
            "author": {},
            "coauthor": {},
        }


class TestPrefetchRelated:
    class View(mixins.PrefetchRelatedMixin, SingleObjectMixin):
        model = Article
        prefetch_related = ["foo", "bar"]

    def test_prefetch_related(self):
        assert self.View().get_prefetch_related() == ["foo", "bar"]

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
        assert view.get_queryset()._prefetch_related_lookups == (
            "coauthor",
            "author",
        )


class TestOrderableList:
    class View(mixins.OrderableListMixin, MultipleObjectMixin):
        model = Article
        orderable_fields = ("author", "title")
        orderable_field_default = "author"

    def test_orderable_aliases(self):
        """Test that the old aliases still work"""

        class OldView(mixins.OrderableListMixin, MultipleObjectMixin):
            model = Article
            orderable_columns = "name"
            orderable_columns_default = "age"
            ordering_default = "desc"

        assert OldView().get_orderable_fields() == "name"
        assert OldView().get_orderable_field_default() == "age"
        assert OldView().get_orderable_direction_default() == "desc"

    def test_request_ordering(self, rf):
        request = rf.get("/?order_by=foo&order_dir=desc")
        view = self.View()
        view.request = request
        assert view.get_order_from_request() == ("foo", "desc")

    def test_queryset_ordering(self, rf):
        request = rf.get("/")
        view = self.View()
        view.request = request
        assert view.get_queryset().query.order_by == ("author",)

    def test_queryset_ordering_with_request(self, rf):
        request = rf.get("/?order_dir=desc")
        view = self.View()
        view.request = request
        assert view.get_queryset().query.order_by == ("-author",)
