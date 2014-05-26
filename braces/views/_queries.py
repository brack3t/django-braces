from django.contrib.sites.models import Site
from django.core.exceptions import ImproperlyConfigured


class SelectRelatedMixin(object):
    """
    Mixin allows you to provide a tuple or list of related models to
    perform a select_related on.
    """
    select_related = None  # Default related fields to none

    def get_queryset(self):
        if self.select_related is None:  # If no fields were provided,
                                         # raise a configuration error
            raise ImproperlyConfigured(
                '{0} is missing the select_related property. This must be '
                'a tuple or list.'.format(self.__class__.__name__))

        if not isinstance(self.select_related, (tuple, list)):
            # If the select_related argument is *not* a tuple or list,
            # raise a configuration error.
            raise ImproperlyConfigured(
                "{0}'s select_related property must be a tuple or "
                "list.".format(self.__class__.__name__))

        # Get the current queryset of the view
        queryset = super(SelectRelatedMixin, self).get_queryset()

        return queryset.select_related(*self.select_related)


class PrefetchRelatedMixin(object):
    """
    Mixin allows you to provide a tuple or list of related models to
    perform a prefetch_related on.
    """
    prefetch_related = None  # Default prefetch fields to none

    def get_queryset(self):
        if self.prefetch_related is None:  # If no fields were provided,
                                           # raise a configuration error
            raise ImproperlyConfigured(
                '{0} is missing the prefetch_related property. This must be '
                'a tuple or list.'.format(self.__class__.__name__))

        if not isinstance(self.prefetch_related, (tuple, list)):
            # If the prefetch_related argument is *not* a tuple or list,
            # raise a configuration error.
            raise ImproperlyConfigured(
                "{0}'s prefetch_related property must be a tuple or "
                "list.".format(self.__class__.__name__))

        # Get the current queryset of the view
        queryset = super(PrefetchRelatedMixin, self).get_queryset()

        return queryset.prefetch_related(*self.prefetch_related)


class OrderableListMixin(object):
    """
    Mixin allows your users to order records using GET parameters
    """

    orderable_columns = None
    orderable_columns_default = None
    order_by = None
    ordering = None

    def get_context_data(self, **kwargs):
        """
        Augments context with:

            * ``order_by`` - name of the field
            * ``ordering`` - order of ordering, either ``asc`` or ``desc``
        """
        context = super(OrderableListMixin, self).get_context_data(**kwargs)
        context["order_by"] = self.order_by
        context["ordering"] = self.ordering
        return context

    def get_orderable_columns(self):
        if not self.orderable_columns:
            raise ImproperlyConfigured(
                '{0} needs the ordering columns defined.'.format(
                    self.__class__.__name__))
        return self.orderable_columns

    def get_orderable_columns_default(self):
        if not self.orderable_columns_default:
            raise ImproperlyConfigured(
                '{0} needs the default ordering column defined.'.format(
                    self.__class__.__name__))
        return self.orderable_columns_default

    def get_ordered_queryset(self, queryset=None):
        """
        Augments ``QuerySet`` with order_by statement if possible

        :param QuerySet queryset: ``QuerySet`` to ``order_by``
        :return: QuerySet
        """
        get_order_by = self.request.GET.get("order_by")

        if get_order_by in self.get_orderable_columns():
            order_by = get_order_by
        else:
            order_by = self.get_orderable_columns_default()

        self.order_by = order_by
        self.ordering = "asc"

        if order_by and self.request.GET.get("ordering", "asc") == "desc":
            order_by = "-" + order_by
            self.ordering = "desc"

        return queryset.order_by(order_by)

    def get_queryset(self):
        """
        Returns ordered ``QuerySet``
        """
        unordered_queryset = super(OrderableListMixin, self).get_queryset()
        return self.get_ordered_queryset(unordered_queryset)


class CurrentSiteMixin(object):
    site_field = 'site'

    def get_queryset(self):
        queryset = super(CurrentSiteMixin, self).get_queryset()
        queryset = queryset.filter(
            **{self.site_field: Site.objects.get_current()})
        return queryset
