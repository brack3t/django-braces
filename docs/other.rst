django-braces Other Mixins
==========================

These mixins handle other random bits of Django's views, like controlling output, controlling content types, or setting values in the context.

.. contents::

SetHeadlineMixin
================

The ``SetHeadlineMixin`` is a newer edition to our client's CMS. It allows us to *statically* or *programmatically* set the headline of any
of our views. We like to write as few templates as possible, so a mixin like this helps us reuse generic templates. Its usage is amazingly
straightforward and works much like Django's built-in ``get_queryset`` method. This mixin has two ways of being used.

Static Example
--------------

::

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        headline = "This is our headline"
        template_name = "path/to/template.html"


Dynamic Example
---------------

::

    from datetime import date

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        template_name = "path/to/template.html"

        def get_headline(self):
            return u"This is our headline for %s" % date.today().isoformat()

In both usages, in the template, just print out ``{{ headline }}`` to show the generated headline.




SelectRelatedMixin
==================

A simple mixin which allows you to specify a list or tuple of foreign key fields to perform a `select_related`_ on.
See Django's docs for more information on `select_related`_.

::

    # views.py
    from django.views.generic import DetailView

    from braces.views import SelectRelatedMixin

    from profiles.models import Profile


    class UserProfileView(SelectRelatedMixin, DetailView):
        model = Profile
        select_related = ["user"]
        template_name = "profiles/detail.html"


PrefetchRelatedMixin
==================

A simple mixin which allows you to specify a list or tuple of reverse foreign key or ManyToMany fields to perform a `prefetch_related`_ on.
See Django's docs for more information on `prefetch_related`_.

::

    # views.py
    from django.contrib.auth.models import User
    from django.views.generic import DetailView

    from braces.views import PrefetchRelatedMixin


    class UserView(PrefetchRelatedMixin, DetailView):
        model = User
        prefetch_related = ["post_set"]  # where the Post model has an FK to the User model as an author.
        template_name = "users/detail.html"


JSONResponseMixin
=================

A simple mixin to handle very simple serialization as a response to the browser.

::

    # views.py
    from django.views.generic import DetailView

    from braces.views import JSONResponseMixin

    class UserProfileAJAXView(JSONResponseMixin, DetailView):
        model = Profile
        json_dumps_kwargs = {'indent': 2}

        def get(self, request, *args, **kwargs):
            self.object = self.get_object()

            context_dict = {
                'name': self.object.user.name,
                'location': self.object.location
            }

            return self.render_json_response(context_dict)

You can additionally use the `AjaxResponseMixin`

::

    # views.py
    from braces.views import AjaxResponseMixin

    class UserProfileView(JSONResponseMixin, AjaxResponseMixin, DetailView):
        model = Profile

        def get_ajax(self, request, *args, **kwargs):
            return self.render_json_object_response(self.get_object())

The `JSONResponseMixin` provides a class-level variable to control the response
type as well. By default it is `application/json`, but you can override that by
providing the `content_type` variable a different value or, programmatically, by
overriding the `get_content_type()` method.

::

    from braces.views import JSONResponseMixin

    class UserProfileAJAXView(JSONResponseMixin, DetailView):
        content_type = 'application/javascript'
        model = Profile

        def get(self, request, *args, **kwargs):
            self.object = self.get_object()

            context_dict = {
                'name': self.object.user.name,
                'location': self.object.location
            }

            return self.render_json_response(context_dict)

        def get_content_type(self):
            # Shown just for illustrative purposes
            return 'application/javascript'

AjaxResponseMixin
=================

A mixin to allow you to provide alternative methods for handling AJAX requests.

To control AJAX-specific behavior, override `get_ajax`, `post_ajax`, `put_ajax`,
or `delete_ajax`. All four methods take `request`, `*args`, and `**kwargs` like
the standard view methods.

::

    # views.py
    from django.views.generic import View

    from braces.views import AjaxResponseMixin, JSONResponseMixin

    class SomeView(JSONResponseMixin, AjaxResponseMixin, View):
        def get_ajax(self, request, *args, **kwargs):
            json_dict = {
                'name': "Benny's Burritos",
                'location': "New York, NY"
            }
            return self.render_json_response(json_dict)


OrderableListMixin
==================

A mixin to allow easy ordering of your queryset basing on the GET parameters.
Works with `ListView`.

To use it, define columns that the data can be order by as well as the default
column to order by in your view. This can be done either by simply setting
the class attributes...

::

    # views.py
    class OrderableListView(OrderableListMixin, ListView):
        model = Article
        orderable_columns = ('id', 'title',)
        orderable_columns_default = 'id'

...or by using similarly name methods to set the ordering constraints more
dynamically:

::

    # views.py
    class OrderableListView(OrderableListMixin, ListView):
        model = Article

        def get_orderable_columns(self):
            # return an iterable
            return ('id', 'title', )

        def get_orderable_columns_default(self):
            # return a string
            return 'id'

The `orderable_columns` restriction is here in order to stop your users from
launching inefficient queries, like ordering by binary columns.

`OrderableListMixin` will order your queryset basing on following GET params:

    * `order_by`: column name, e.g. `'title'`
    * `ordering`: `'asc'` (default) or `'desc'`

Example url: http://127.0.0.1:8000/articles/?order_by=title&ordering=asc

.. _select_related: https://docs.djangoproject.com/en/1.5/ref/models/querysets/#select-related
.. _prefetch_related: https://docs.djangoproject.com/en/1.5/ref/models/querysets/#prefetch-related
