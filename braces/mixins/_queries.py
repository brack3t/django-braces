from typing import Iterable
import warnings

from django.core.exceptions import ImproperlyConfigured


class SelectRelatedMixin:
    select_related: str | Iterable[str] = None

    def get_select_related(self) -> list[str]:
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
    prefetch_related: str | Iterable[str] = None

    def get_prefetch_related(self) -> list[str]:
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