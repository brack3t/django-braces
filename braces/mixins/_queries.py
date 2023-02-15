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