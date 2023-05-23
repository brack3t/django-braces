Query Mixins
============

The plugins in this module are all related to the queries made by class-based
views.

.. _SelectRelatedMixin:

SelectRelatedMixin
------------------

This mixin makes it easier to added ``select_related`` clauses to your
querysets. See Django's docs for more information on `select_related`_.

::

    # views.py
    from django.views.generic import DetailView

    from braces.mixins import SelectRelatedMixin

    from profiles.models import Profile


    class UserProfileView(SelectRelatedMixin, DetailView):
        model = Profile
        select_related = ["user"]
        template_name = "profiles/detail.html"

The above view will produce a queryset that looks like this::

    Profile.objects.select_related("user")


.. _PrefetchRelatedMixin:

PrefetchRelatedMixin
--------------------

This mixin makes it easier to added ``prefetch_related`` clauses to your
querysets. See Django's docs for more information on `prefetch_related`_.

::

    # views.py
    from django.contrib.auth.models import User
    from django.views.generic import DetailView

    from braces.mixins import PrefetchRelatedMixin


    class UserView(PrefetchRelatedMixin, DetailView):
        model = User
        prefetch_related = ["post_set"]
        template_name = "users/detail.html"

The above view will produce a queryset that looks like this::

    User.objects.prefetch_related("post_set")


.. _OrderableListMixin:

OrderableListMixin
------------------

This mixin provides your views with the ability to sort their querysets by
columns and order them by ascending or descending order, all based on URL
parameters.

The mixin provides three fields to control the ordering:
* ``orderable_fields`` is a list of fields to allow ordering by
* ``orderable_field_default`` is the default field to order by
* ``orderable_direction_default`` is the default direction to order by

For backwards compatibility, the mixin also supports the following fields:
``orderable_columns``, ``orderable_columns_default`` and ``ordering_default``.
These correspond to the above fields, in order.

::

    # views.py
    from django.views import ListView

    from braces.mixins import OrderableListMixin


    class OrderableListView(OrderableListMixin, ListView):
        model = Article
        orderable_fields = ("id", "title",)
        orderable_field_default = "id"
        orderable_direction_default = "asc"

The ``orderable_fields`` restriction is here in order to stop your users
from launching inefficient queries, like ordering by binary columns.

``OrderableListMixin`` will order your queryset basing on following GET params:

    * ``order_by``: column name, e.g. ``"title"``
    * ``order_dir``: ``"asc"`` (default) or ``"desc"``

Example url: `http://127.0.0.1:8000/articles/?order_by=title&ordering=asc`


.. _select_related: https://docs.djangoproject.com/en/dev/ref/models/querysets/#select-related
.. _prefetch_related: https://docs.djangoproject.com/en/dev/ref/models/querysets/#prefetch-related
