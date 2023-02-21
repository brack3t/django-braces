from __future__ import annotations

import warnings
from typing import Iterable, Union

from django.core.exceptions import ImproperlyConfigured
from django.db.models import QuerySet


class SelectRelatedMixin:
    """A mixin for adding select_related to the queryset"""

    select_related: Union[str, Iterable[str]] = None

    def get_select_related(self) -> list[str]:
        """Get the fields to be select_related"""
        if getattr(self, "select_related", None) is None:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} is missing the select_related "
                "attribute."
            )
        if not self.select_related:
            warnings.warn("The select_related attribute is empty")

        if not isinstance(self.select_related, (tuple, list)):
            self.select_related = [self.select_related]

        return self.select_related

    def get_queryset(self) -> QuerySet:
        """Add select_related to the queryset"""
        queryset = super().get_queryset()
        select_related = self.get_select_related()
        return queryset.select_related(*select_related)


class PrefetchRelatedMixin:
    """A mixin for adding prefetch_related to the queryset"""

    prefetch_related: Union[str, Iterable[str]] = None

    def get_prefetch_related(self) -> list[str]:
        """Get the fields to be prefetch_related"""
        if getattr(self, "prefetch_related", None) is None:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} is missing the prefetch_related attribute."
            )
        if not self.prefetch_related:
            warnings.warn("The prefetch_related attribute is empty")

        if not isinstance(self.prefetch_related, (tuple, list)):
            self.prefetch_related = [self.prefetch_related]

        return self.prefetch_related

    def get_queryset(self) -> QuerySet:
        """Add prefetch_related to the queryset"""
        queryset = super().get_queryset()
        prefetch_related = self.get_prefetch_related()
        return queryset.prefetch_related(*prefetch_related)


class OrderableListMixin:
    """A mixin for adding query-string based ordering to the queryset."""

    orderable_fields: list[str] = None
    orderable_field_default: str = None
    orderable_direction_default: str = "asc"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        if getattr(self, "orderable_columns", None) is not None:
            self.orderable_fields = self.orderable_columns
        if getattr(self, "orderable_columns_default", None) is not None:
            self.orderable_field_default = self.orderable_columns_default
        if getattr(self, "ordering_default", None) is not None:
            self.orderable_direction_default = self.ordering_default

    def get_orderable_fields(self) -> list[str]:
        """Which fields can be used for ordering?"""
        if not self.orderable_fields:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} needs the orderable fields defined."
            )
        return self.orderable_fields

    def get_orderable_field_default(self) -> str:
        """Which field should be used for ordering by default?"""
        if not self.orderable_field_default:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} needs the default orderable field defined."
            )
        return self.orderable_field_default

    def get_orderable_direction_default(self) -> str:
        """Which direction should be used for ordering by default?"""
        direction = self.orderable_direction_default
        if not direction or direction not in ["asc", "desc"]:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} only allows asc or desc as orderable direction default"
            )
        return direction

    def get_order_from_request(self) -> Iterable[str]:
        """Use the query string to determine the ordering."""
        request_kwargs = self.request.GET.dict()
        field = request_kwargs.get("order_by", "").lower()
        direction = request_kwargs.get("order_dir", "").lower()

        if not field:
            field = self.get_orderable_field_default()
        if not direction:
            direction = self.get_orderable_direction_default()
        return field, direction

    def get_queryset(self) -> QuerySet:
        """Order the queryset."""
        queryset = super().get_queryset()

        field, direction = self.get_order_from_request()
        allowed_fields = self.get_orderable_fields()

        if direction == "desc":
            direction = "-"
        else:
            direction = ""

        if field in allowed_fields:
            return queryset.order_by(f"{direction}{field}")

        return queryset
