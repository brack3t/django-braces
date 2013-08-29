Other Mixins
============

These mixins handle other random bits of Django's views, like controlling output, controlling content types, or setting values in the context.

.. contents::

.. _SetHeadlineMixin:

SetHeadlineMixin
----------------

The ``SetHeadlineMixin`` is a newer edition to our client's CMS. It allows us to *statically* or *programmatically* set the headline of any of our views. We like to write as few templates as possible, so a mixin like this helps us reuse generic templates. Its usage is amazingly straightforward and works much like Django's built-in ``get_queryset`` method. This mixin has two ways of being used.

Static Example
^^^^^^^^^^^^^^

::

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        headline = "This is our headline"
        template_name = "path/to/template.html"


Dynamic Example
^^^^^^^^^^^^^^^

::

    from datetime import date

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        template_name = "path/to/template.html"

        def get_headline(self):
            return u"This is our headline for %s" % date.today().isoformat()

In both usages, in the template, just print out ``{{ headline }}`` to show the generated headline.




.. _SelectRelatedMixin:

SelectRelatedMixin
------------------

A simple mixin which allows you to specify a list or tuple of foreign key fields to perform a `select_related`_ on.  See Django's docs for more information on `select_related`_.

::

    # views.py
    from django.views.generic import DetailView

    from braces.views import SelectRelatedMixin

    from profiles.models import Profile


    class UserProfileView(SelectRelatedMixin, DetailView):
        model = Profile
        select_related = ["user"]
        template_name = "profiles/detail.html"


.. _PrefetchRelatedMixin:

PrefetchRelatedMixin
------------------

A simple mixin which allows you to specify a list or tuple of reverse foreign key or ManyToMany fields to perform a `prefetch_related`_ on. See Django's docs for more information on `prefetch_related`_.

::

    # views.py
    from django.contrib.auth.models import User
    from django.views.generic import DetailView

    from braces.views import PrefetchRelatedMixin


    class UserView(PrefetchRelatedMixin, DetailView):
        model = User
        prefetch_related = ["post_set"]  # where the Post model has an FK to the User model as an author.
        template_name = "users/detail.html"


.. _JSONResponseMixin:

JSONResponseMixin
-----------------

.. versionchanged:: 1.1
    ``render_json_response`` now accepts a ``status_code`` keyword argument.
    ``json_dumps_kwargs`` class-attribute and ``get_json_dumps_kwargs`` method to provide arguments to the ``json.dumps()`` method.

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

.. _JsonRequestResponseMixin:

JsonRequestResponseMixin
------------------------

.. versionadded:: 1.3

A mixin that attempts to parse request as JSON.  If request is properly formatted, the json is saved to self.request_json as a Python object.  request_json will be None for imparsible requests.

To catch requests that aren't JSON-formatted, set the class attribute ``require_json`` to True.

Override the class attribute ``error_response_dict`` to customize the default error message.

It extends :ref:`JSONResponseMixin`, so those utilities are available as well.

Note: To allow public access to your view, you'll need to use the ``csrf_exempt`` decorator or :ref:`CsrfExemptMixin`.

::

    from django.views.generic import View

    from braces.views import CsrfExemptMixin, JsonRequestResponseMixin

    class SomeView(CsrfExemptMixin, JsonRequestResponseMixin, View):
        require_json = True

        def post(self, request, *args, **kwargs):
            try:
                burrito = self.request_json['burrito']
                toppings = self.request_json['toppings']
            except:
                error_dict = {'message':
                    'your order must include a burrito AND toppings'}
                return self.render_bad_request_response(error_dict)
            place_order(burrito, toppings)
            return self.render_json_response(
                {'message': 'Your order has been placed!'})


.. _AjaxResponseMixin:

AjaxResponseMixin
-----------------

A mixin to allow you to provide alternative methods for handling AJAX requests.

To control AJAX-specific behavior, override ``get_ajax``, ``post_ajax``, ``put_ajax``, or ``delete_ajax``. All four methods take ``request``, ``*args``, and ``**kwargs`` like the standard view methods.

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


.. _OrderableListMixin:

OrderableListMixin
------------------

.. versionadded:: 1.1

A mixin to allow easy ordering of your queryset basing on the GET parameters. Works with `ListView`.

To use it, define columns that the data can be order by as well as the default column to order by in your view. This can be done either by simply setting the class attributes...

::

    # views.py
    class OrderableListView(OrderableListMixin, ListView):
        model = Article
        orderable_columns = ('id', 'title',)
        orderable_columns_default = 'id'

...or by using similarly name methods to set the ordering constraints more dynamically:

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

The ``orderable_columns`` restriction is here in order to stop your users from launching inefficient queries, like ordering by binary columns.

``OrderableListMixin`` will order your queryset basing on following GET params:

    * ``order_by``: column name, e.g. ``'title'``
    * ``ordering``: `'asc'` (default) or ``'desc'``

Example url: `http://127.0.0.1:8000/articles/?order_by=title&ordering=asc`


.. _CanonicalSlugDetailMixin:

CanonicalSlugDetailMixin
------------------------

.. versionadded:: 1.3

A mixin that enforces a canonical slug in the url. Works with ``DetailView``.

If a urlpattern takes a object's pk and slug as arguments and the slug url argument does not equal the object's canonical slug, this mixin will redirect to the url containing the canonical slug.

To use it, the urlpattern must accept both a ``pk`` and ``slug`` argument in its regex:

::

    # urls.py
    urlpatterns = patterns('',
        url(r'^article/(?P<pk>\d+)-(?P<slug>[-\w]+)$')
        ArticleView.as_view(),
        'view_article'
    )

Then create a standard DetailView that inherits this mixin:

::

    class ArticleView(CanonicalSlugDetailMixin, DetailView):
        model = Article

Now, given an Article object with ``{pk: 1, slug: 'hello-world'}``, the url `http://127.0.0.1:8000/article/1-goodbye-moon` will redirect to `http://127.0.0.1:8000/article/1-hello-world` with the HTTP status code 301 Moved Permanently. Any other non-canonical slug, not just 'goodbye-moon', will trigger the redirect as well.

Control the canonical slug by either implementing the method ``get_canonical_slug()`` on the model class:

::

    class Article(models.Model):
        blog = models.ForeignKey('Blog')
        slug = models.SlugField()

        def get_canonical_slug(self):
          return "{}-{}".format(self.blog.get_canonical_slug(), self.slug)

Or by overriding the ``get_canonical_slug()`` method on the view:

::

    class ArticleView(CanonicalSlugDetailMixin, DetailView):
        model = Article

        def get_canonical_slug():
            import codecs
            return codecs.encode(self.get_object().slug, 'rot_13')

Given the same Article as before, this will generate urls of `http://127.0.0.1:8000/article/1-my-blog-hello-world` and `http://127.0.0.1:8000/article/1-uryyb-jbeyq`, respectively.

.. _select_related: https://docs.djangoproject.com/en/1.5/ref/models/querysets/#select-related
.. _prefetch_related: https://docs.djangoproject.com/en/1.5/ref/models/querysets/#prefetch-related
