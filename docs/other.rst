Other Mixins
============

These mixins handle other random bits of Django's views, like controlling output, controlling content types, or setting values in the context.

.. contents::

.. _SetHeadlineMixin:

SetHeadlineMixin
----------------

The ``SetHeadlineMixin`` allows you to *statically* or *programmatically* set the headline of any of your views. Ideally, you'll write as few templates as possible, so a mixin like this helps you reuse generic templates. Its usage is amazingly straightforward and works much like Django's built-in ``get_queryset`` method. This mixin has two ways of being used:

Static Example
^^^^^^^^^^^^^^

::

    from django.utils.translation import ugettext_lazy as _
    from django.views import TemplateView

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        headline = _(u"This is our headline")
        template_name = u"path/to/template.html"


Dynamic Example
^^^^^^^^^^^^^^^

::

    from datetime import date

    from django.views import TemplateView

    from braces.views import SetHeadlineMixin


    class HeadlineView(SetHeadlineMixin, TemplateView):
        template_name = u"path/to/template.html"

        def get_headline(self):
            return u"This is our headline for {0}".format(date.today().isoformat())

For both usages, the context now contains a ``headline`` key with your headline.


.. _StaticContextMixin:

StaticContextMixin
------------------

.. versionadded:: 1.4

The ``StaticContextMixin`` allows you to easily set static context data by using the ``static_context`` property.

.. note::
    While it's possible to override the ``StaticContextMixin.get_static_context method``, it's not very practical. If you have a need to override a method for dynamic context data it's best to override the standard ``get_context_data`` method of Django's generic class-based views.


View Example
^^^^^^^^^^^^

::

    # views.py

    from django.views import TemplateView

    from braces.views import StaticContextMixin


    class ContextTemplateView(StaticContextMixin, TemplateView):
        static_context = {u"nav_home": True}


URL Example
^^^^^^^^^^^

::

    # urls.py

    urlpatterns = patterns(
        '',
        url(ur"^$",
            ContextTemplateView.as_view(
                template_name=u"index.html",
                static_context={u"nav_home": True}
            ),
            name=u"index")
    )


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
        select_related = [u"user"]
        template_name = u"profiles/detail.html"

.. _select_related: https://docs.djangoproject.com/en/dev/ref/models/querysets/#select-related


.. _PrefetchRelatedMixin:

PrefetchRelatedMixin
--------------------

A simple mixin which allows you to specify a list or tuple of reverse foreign key or ManyToMany fields to perform a `prefetch_related`_ on. See Django's docs for more information on `prefetch_related`_.

::

    # views.py
    from django.contrib.auth.models import User
    from django.views.generic import DetailView

    from braces.views import PrefetchRelatedMixin


    class UserView(PrefetchRelatedMixin, DetailView):
        model = User
        prefetch_related = [u"post_set"]  # where the Post model has an FK to the User model as an author.
        template_name = u"users/detail.html"

.. _prefetch_related: https://docs.djangoproject.com/en/dev/ref/models/querysets/#prefetch-related


.. _JSONResponseMixin:

JSONResponseMixin
-----------------

.. versionchanged:: 1.1
    ``render_json_response`` now accepts a ``status`` keyword argument.
    ``json_dumps_kwargs`` class-attribute and ``get_json_dumps_kwargs`` method to provide arguments to the ``json.dumps()`` method.

A simple mixin to handle very simple serialization as a response to the browser.

::

    # views.py
    from django.views.generic import DetailView

    from braces.views import JSONResponseMixin

    class UserProfileAJAXView(JSONResponseMixin, DetailView):
        model = Profile
        json_dumps_kwargs = {u"indent": 2}

        def get(self, request, *args, **kwargs):
            self.object = self.get_object()

            context_dict = {
                u"name": self.object.user.name,
                u"location": self.object.location
            }

            return self.render_json_response(context_dict)

You can additionally use the `AjaxResponseMixin`

::

    # views.py
    from django.views import DetailView

    from braces import views


    class UserProfileView(views.JSONResponseMixin,
                          views.AjaxResponseMixin,
                          DetailView):
        model = Profile

        def get_ajax(self, request, *args, **kwargs):
            return self.render_json_object_response(self.get_object())

The `JSONResponseMixin` provides a class-level variable to control the response
type as well. By default it is `application/json`, but you can override that by
providing the `content_type` variable a different value or, programmatically, by
overriding the `get_content_type()` method.

::

    from django.views import DetailView

    from braces.views import JSONResponseMixin


    class UserProfileAJAXView(JSONResponseMixin, DetailView):
        content_type = u"application/javascript"
        model = Profile

        def get(self, request, *args, **kwargs):
            self.object = self.get_object()

            context_dict = {
                u"name": self.object.user.name,
                u"location": self.object.location
            }

            return self.render_json_response(context_dict)

        def get_content_type(self):
            # Shown just for illustrative purposes
            return u"application/javascript"

The `JSONResponseMixin` provides another class-level variable
`json_encoder_class` to use a custom json encoder with `json.dumps`.
By default it is `django.core.serializers.json.DjangoJsonEncoder`

::

    from django.core.serializers.json import DjangoJSONEncoder

    from braces.views import JSONResponseMixin


    class SetJSONEncoder(DjangoJSONEncoder):
        """
        A custom JSONEncoder extending `DjangoJSONEncoder` to handle serialization
        of `set`.
        """
        def default(self, obj):
            if isinstance(obj, set):
                return list(obj)
            return super(DjangoJSONEncoder, self).default(obj)


    class GetSetDataView(JSONResponseMixin, View):
        json_encoder_class = SetJSONEncoder

        def get(self, request, *args, **kwargs):
            numbers_set = set(range(10))
            data = {'numbers': numbers_set}
            return self.render_json_response(data)

.. _JsonRequestResponseMixin:

JsonRequestResponseMixin
------------------------

.. versionadded:: 1.3

A mixin that attempts to parse the request as JSON.  If the request is properly formatted, the JSON is saved to ``self.request_json`` as a Python object.  ``request_json`` will be ``None`` for imparsible requests.

To catch requests that aren't JSON-formatted, set the class attribute ``require_json`` to ``True``.

Override the class attribute ``error_response_dict`` to customize the default error message.

It extends :ref:`JSONResponseMixin`, so those utilities are available as well.

.. note::
    To allow public access to your view, you'll need to use the ``csrf_exempt`` decorator or :ref:`CsrfExemptMixin`.

::

    from django.views.generic import View

    from braces import views

    class SomeView(views.CsrfExemptMixin, views.JsonRequestResponseMixin, View):
        require_json = True

        def post(self, request, *args, **kwargs):
            try:
                burrito = self.request_json[u"burrito"]
                toppings = self.request_json[u"toppings"]
            except KeyError:
                error_dict = {u"message":
                   u"your order must include a burrito AND toppings"}
                return self.render_bad_request_response(error_dict)
            place_order(burrito, toppings)
            return self.render_json_response(
                {u"message": u"Your order has been placed!"})


.. _AjaxResponseMixin:

AjaxResponseMixin
-----------------

This mixin provides hooks for alternate processing of AJAX requests based on HTTP verb.

To control AJAX-specific behavior, override ``get_ajax``, ``post_ajax``, ``put_ajax``, or ``delete_ajax``. All four methods take ``request``, ``*args``, and ``**kwargs`` like the standard view methods.

::

    # views.py
    from django.views.generic import View

    from braces import views

    class SomeView(views.JSONResponseMixin, views.AjaxResponseMixin, View):
        def get_ajax(self, request, *args, **kwargs):
            json_dict = {
                'name': "Benny's Burritos",
                'location': "New York, NY"
            }
            return self.render_json_response(json_dict)

.. note::
    This mixin is only useful if you need to have behavior in your view fork based on ``request.is_ajax()``.


.. _OrderableListMixin:

OrderableListMixin
------------------

.. versionadded:: 1.1

A mixin to allow easy ordering of your queryset basing on the GET parameters. Works with `ListView`.

To use it, define columns that the data can be ordered by, as well as the default column to order by in your view. This can be done either by simply setting the class attributes:

::

    # views.py
    from django.views import ListView

    from braces.views import OrderableListMixin


    class OrderableListView(OrderableListMixin, ListView):
        model = Article
        orderable_columns = (u"id", u"title",)
        orderable_columns_default = u"id"

Or by using similarly-named methods to set the ordering constraints more dynamically:

::

    # views.py
    from django.views import ListView

    from braces.views import OrderableListMixin


    class OrderableListView(OrderableListMixin, ListView):
        model = Article

        def get_orderable_columns(self):
            # return an iterable
            return (u"id", u"title",)

        def get_orderable_columns_default(self):
            # return a string
            return u"id"

The ``orderable_columns`` restriction is here in order to stop your users from launching inefficient queries, like ordering by binary columns.

``OrderableListMixin`` will order your queryset basing on following GET params:

    * ``order_by``: column name, e.g. ``"title"``
    * ``ordering``: ``"asc"`` (default) or ``"desc"``

Example url: `http://127.0.0.1:8000/articles/?order_by=title&ordering=asc`

You can also override the default ordering from ``"asc"`` to ``"desc"``
by setting the ``"ordering_default"`` in your view class.

::

    # views.py
    from django.views import ListView

    from braces.views import OrderableListMixin


    class OrderableListView(OrderableListMixin, ListView):
        model = Article
        orderable_columns = (u"id", u"title",)
        orderable_columns_default = u"id"
        ordering_default = u"desc"

This will reverse the order of list objects if no query param is given.


**Front-end Example Usage**

If you're using bootstrap you could create a template like the following:

.. code:: html

    <div class="table-responsive">
        <table class="table table-striped table-bordered">
            <tr>
                <th><a class="order-by-column" data-column="id" href="#">ID</a></th>
                <th><a class="order-by-column" data-column="title" href="#">Title</a></th>
            </tr>
            {% for object in object_list %}
                <tr>
                    <td>{{ object.id }}</td>
                    <td>{{ object.title }}</td>
                </tr>
            {% endfor %}
        </table>
    </div>

    <script>
    function setupOrderedColumns(order_by, orderin) {

        $('.order-by-column').each(function() {

            var $el = $(this),
                column_name = $el.data('column'),
                href = location.href,
                next_order = 'asc',
                has_query_string = (href.indexOf('?') !== -1),
                order_by_param,
                ordering_param;

            if (order_by === column_name) {
                $el.addClass('current');
                $el.addClass(ordering);
                $el.append('<span class="caret"></span>');
                if (ordering === 'asc') {
                    $el.addClass('dropup');
                    next_order = 'desc';
                }
            }

            order_by_param = "order_by=" + column_name;
            ordering_param = "ordering=" + next_order;

            if (!has_query_string) {
                href = '?' + order_by_param + '&' + ordering_param;
            } else {
                if (href.match(/ordering=(asc|desc)/)) {
                    href = href.replace(/ordering=(asc|desc)/, ordering_param);
                } else {
                    href += '&' + ordering_param;
                }

                if (href.match(/order_by=[_\w]+/)) {
                    href = href.replace(/order_by=([_\w]+)/, order_by_param);
                } else {
                    href += '&' + order_by_param;
                }

            }

            $el.attr('href', href);

        });
    }
    setupOrderedColumns('{{ order_by }}', '{{ ordering }}');
    </script>

.. _CanonicalSlugDetailMixin:

CanonicalSlugDetailMixin
------------------------

.. versionadded:: 1.3

A mixin that enforces a canonical slug in the URL. Works with ``DetailView``.

If a ``urlpattern`` takes a object's ``pk`` and ``slug`` as arguments and the ``slug`` URL argument does not equal the object's canonical slug, this mixin will redirect to the URL containing the canonical slug.

To use it, the ``urlpattern`` must accept both a ``pk`` and ``slug`` argument in its regex:

::

    # urls.py
    urlpatterns = patterns('',
        url(r"^article/(?P<pk>\d+)-(?P<slug>[-\w]+)$")
        ArticleView.as_view(),
        "view_article"
    )

Then create a standard ``DetailView`` that inherits this mixin:

::

    class ArticleView(CanonicalSlugDetailMixin, DetailView):
        model = Article

Now, given an ``Article`` object with ``{pk: 1, slug: 'hello-world'}``, the URL `http://127.0.0.1:8000/article/1-goodbye-moon` will redirect to `http://127.0.0.1:8000/article/1-hello-world` with the HTTP status code 301 Moved Permanently. Any other non-canonical slug, not just 'goodbye-moon', will trigger the redirect as well.

Control the canonical slug by either implementing the method ``get_canonical_slug()`` on the model class:

::

    class Article(models.Model):
        blog = models.ForeignKey('Blog')
        slug = models.SlugField()

        def get_canonical_slug(self):
          return "{0}-{1}".format(self.blog.get_canonical_slug(), self.slug)

Or by overriding the ``get_canonical_slug()`` method on the view:

::

    class ArticleView(CanonicalSlugDetailMixin, DetailView):
        model = Article

        def get_canonical_slug():
            import codecs
            return codecs.encode(self.get_object().slug, "rot_13")

Given the same Article as before, this will generate urls of `http://127.0.0.1:8000/article/1-my-blog-hello-world` and `http://127.0.0.1:8000/article/1-uryyb-jbeyq`, respectively.


.. _MessageMixin:

MessageMixin
------------

.. versionadded:: 1.4

A mixin that adds a ``messages`` attribute on the view which acts as a wrapper
to ``django.contrib.messages`` and passes the ``request`` object automatically.

    .. warning::
        If you're using Django 1.4, then the ``message`` attribute is only
        available after the base view's ``dispatch`` method has been called
        (so our second example would not work for instance).

::

    from django.views.generic import TemplateView

    from braces.views import MessageMixin


    class MyView(MessageMixin, TemplateView):
        """
        This view will add a debug message which can then be displayed
        in the template.
        """
        template_name = "my_template.html"

        def get(self, request, *args, **kwargs):
            self.messages.debug("This is a debug message.")
            return super(MyView, self).get(request, *args, **kwargs)


::

    from django.contrib import messages
    from django.views.generic import TemplateView

    from braces.views import MessageMixin


    class OnlyWarningView(MessageMixin, TemplateView):
        """
        This view will only show messages that have a level
        above `warning`.
        """
        template_name = "my_template.html"

        def dispatch(self, request, *args, **kwargs):
            self.messages.set_level(messages.WARNING)
            return super(OnlyWarningView, self).dispatch(request, *args, **kwargs)


.. _AllVerbsMixin:

AllVerbsMixin
-------------

.. versionadded:: 1.4

This mixin allows you to specify a single method that will respond to all HTTP verbs, making a class-based view behave much like a function-based view.

::

    from django.views import TemplateView

    from braces.views import AllVerbsMixin


    class JustShowItView(AllVerbsMixin, TemplateView):
        template_name = "just/show_it.html"

        def all(self, request, *args, **kwargs):
            return super(JustShowItView, self).get(request, *args, **kwargs)

If you need to change the name of the method called, provide a new value to the ``all_handler`` attribute (default is ``'all'``)



.. _HeaderMixin:

HeaderMixin
-------------

.. versionadded:: 1.11

This mixin allows you to add arbitrary HTTP header to a response. Static headers can be defined in the ``headers`` attribute of the view.

::

    from django.views import TemplateView

    from braces.views import HeaderMixin


    class StaticHeadersView(HeaderMixin, TemplateView):
        template_name = "some/headers.html"
        headers = {
            'X-Header-Sample': 'some value',
            'X-Some-Number': 42
        }


If you need to set the headers dynamically, e.g depending on some request information, override the ``get_headers`` method instead.

::

    from django.views import TemplateView

    from braces.views import HeaderMixin


    class EchoHeadersView(HeaderMixin, TemplateView):
        template_name = "some/headers.html"

        def get_headers(self, request):
            """
            Echo back request headers with ``X-Request-`` prefix.
            """
            for key, value in request.META.items():
                yield "X-Request-{}".format(key), value



.. _CacheControlMixin:

CacheControlMixin
-----------------------

Mixin that allows setting ``Cache-Control`` header options.

``Cache-Control`` directive explanations:

http://condor.depaul.edu/dmumaugh/readings/handouts/SE435/HTTP/node24.html

Django's ``django.views.decorators.cache.cache_control`` options:

https://docs.djangoproject.com/en/dev/topics/cache/#controlling-cache-using-other-headers

::

    from django.views import TemplateView
    from braces.views import CacheControlMixin


    class MyView(CacheControlMixin, TemplateView):
        template_name = "project/template.html"

        # Specify Cache-Control options as class-level attributes.
        # The arguments are passed to Django's `cache_control()` view decorator
        # directly (with the `cachecontrol_` prefix removed.)
        cachecontrol_public = True
        cachecontrol_max_age = 60

        # Alternatively, you can implement the ``get_cachecontrol_options()``
        # classmethod that should return a dictionary of the kwargs passed to
        # Django's `cache_control()` view decorator.
        @classmethod
        def get_cachecontrol_options(cls):
            return {'public': True, 'max_age': 60}


This will cause the following header to be emitted for every request to this view:

::

    Cache-Control: public, max-age=60


Available ``Cache-Control`` options:

::

    # These are all `None` by default, which indicates unset.
    cachecontrol_public = None
    cachecontrol_private = None
    cachecontrol_no_cache = None
    cachecontrol_no_transform = None
    cachecontrol_must_revalidate = None
    cachecontrol_proxy_revalidate = None
    cachecontrol_max_age = None
    cachecontrol_s_maxage = None


.. _NeverCacheMixin:

NeverCacheMixin
-----------------------

Mixin that applies Django's ``django.views.decorators.cache.never_cache`` view decorator to prevent upstream HTTP-based caching.

::

    from django.views import TemplateView
    from braces.views import NeverCacheMixin


    class MyView(NeverCacheMixin, TemplateView):
        template_name = "project/template.html"


This will cause the following header to be emitted for every request to this view:

::

    Cache-Control: max-age=0, no-cache, no-store, must-revalidate
