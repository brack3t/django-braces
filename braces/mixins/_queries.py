from typing import Iterable, Union, List
import warnings

from django.core.exceptions import ImproperlyConfigured


class SelectRelatedMixin:
    select_related: Union[str, Iterable[str]] = None

    def get_select_related(self) -> List[str]:
        if getattr(self, "select_related", None) is None:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} is missing the select_related attribute."
            )
        if not self.select_related:
            warnings.warn("The select_related attribute is empty")

        if not isinstance(self.select_related, (tuple, list)):
            self.select_related = [self.select_related]

        return self.select_related

    def get_queryset(self) -> "QuerySet":
        queryset = super().get_queryset()
        select_related = self.get_select_related()
        return queryset.select_related(*select_related)


class PrefetchRelatedMixin:
    prefetch_related: Union[str, Iterable[str]] = None

    def get_prefetch_related(self) -> List[str]:
        if getattr(self, "prefetch_related", None) is None:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} is missing the prefetch_related attribute."
            )
        if not self.prefetch_related:
            warnings.warn("The prefetch_related attribute is empty")

        if not isinstance(self.prefetch_related, (tuple, list)):
            self.prefetch_related = [self.prefetch_related]

        return self.prefetch_related

    def get_queryset(self) -> "QuerySet":
        queryset = super().get_queryset()
        prefetch_related = self.get_prefetch_related()
        return queryset.prefetch_related(*prefetch_related)


class OrderableListMixin:
    orderable_fields: List[str] = None
    orderable_field_default: str = None
    orderable_direction_default: str = "asc"

    def __init__(self, *args, **kwargs):
        if getattr(self, "orderable_columns", None):
            self.orderable_fields = self.orderable_columns
        if getattr(self, "orderable_columns_default", None):
            self.orderable_field_default = self.orderable_columns_default
        if getattr(self, "ordering_default", None):
            self.orderable_direction_default = self.ordering_default
        super().__init__(*args, **kwargs)

    def get_orderable_fields(self) -> List[str]:
        if not self.orderable_fields:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} needs the ordering columns defined."
            )
        return self.orderable_fields

    def get_orderable_field_default(self) -> str:
        if not self.orderable_field_default:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} needs the default ordering column defined."
            )
        return self.orderable_field_default

    def get_orderable_direction_default(self) -> str:
        direction = self.orderable_direction_default
        if not direction or direction not in ["asc", "desc"]:
            raise ImproperlyConfigured(
                f"{self.__class__.__name__} only allows asc or desc as ordering option"
            )
        return direction

    def get_order_from_request(self) -> Iterable[str]:
        field = self.request.kwargs.get("order_by", "").lower()
        direction = self.request.kwargs.get("order_dir", "").lower()

        if not field:
            field = self.get_orderable_fields_default()
        if not direction:
            direction = self.get_orderable_direction_default()
        return field, direction

    def get_queryset(self) -> "QuerySet":
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
