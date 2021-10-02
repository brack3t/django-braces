from django.core.exceptions import ImproperlyConfigured
from django.shortcuts import redirect
from django.utils import timezone
from django.views.decorators.cache import cache_page, never_cache
from django.core.paginator import (Paginator, EmptyPage, PageNotAnInteger)
try:
    from django.utils.encoding import force_str as force_string
except ImportError:
    from django.utils.encoding import force_text as force_string
try:
    from django.urls import resolve
except ImportError:
    from django.core.urlresolvers import resolve
import pytz


class PaginatorMixin(object):
    def __init__(self, queryset, numb_pages, request_page):
        self.queryset       = queryset          #egg: models.Post.objects.all()
        self.numb_pages     = numb_pages        #egg: int 10 `is number per page`
        self.request_page   = request_page      #egg: request.GET.get('page')

    def page_numbering(self):
        paginator = Paginator(self.queryset, self.numb_pages)
        try:
            pagination = paginator.page(self.request_page)
        except PageNotAnInteger:
            pagination = paginator.page(1)
        except EmptyPage:
            pagination = paginator.page(paginator.num_pages)

        index       = pagination.number - 1
        limit       = 5 #limit for show range left and right of number pages
        max_index   = len(paginator.page_range)
        start_index = index - limit if index >= limit else 0
        end_index   = index + limit if index <= max_index - limit else max_index

        page_range  = list(paginator.page_range)[start_index:end_index]
        return page_range

    def queryset_paginated(self):
        paginator = Paginator(self.queryset, self.numb_pages)
        try:
            queryset_paginated = paginator.page(self.request_page)
        except PageNotAnInteger:
            queryset_paginated = paginator.page(1)
        except EmptyPage:
            queryset_paginated = paginator.page(paginator.num_pages)

        return queryset_paginated


class TimezoneMixin(object):
    def process_request(self, request):
        tzname = request.session.get('django_timezone')
        if tzname:
            timezone.activate(pytz.timezone(tzname))
        else:
            timezone.deactivate()


class NeverCacheMixin(object):
    @method_decorator(never_cache)
    def dispatch(self, *args, **kwargs):
        return super(NeverCacheMixin, self).dispatch(*args, **kwargs)


class CacheMixin(object):
    cache_timeout = 60

    def get_cache_timeout(self):
        return self.cache_timeout

    def dispatch(self, *args, **kwargs):
        return cache_page(self.get_cache_timeout())(super(CacheMixin, self).dispatch)(*args, **kwargs)


class CacheControlMixin(object):
    cache_timeout = 60

    def get_cache_timeout(self):
        return self.cache_timeout

    def dispatch(self, *args, **kwargs):
        response = super(CacheControlMixin, self).dispatch(*args, **kwargs)
        patch_response_headers(response, self.get_cache_timeout())
        return response


class SetHeadlineMixin(object):
    """
    Mixin allows you to set a static headline through a static property on the
    class or programmatically by overloading the get_headline method.
    """
    headline = None  # Default the headline to none

    def get_context_data(self, **kwargs):
        kwargs = super(SetHeadlineMixin, self).get_context_data(**kwargs)
        # Update the existing context dict with the provided headline.
        kwargs.update({"headline": self.get_headline()})
        return kwargs

    def get_headline(self):
        if self.headline is None:  # If no headline was provided as a view
                                   # attribute and this method wasn't
                                   # overridden raise a configuration error.
            raise ImproperlyConfigured(
                '{0} is missing a headline. '
                'Define {0}.headline, or override '
                '{0}.get_headline().'.format(self.__class__.__name__))
        return force_string(self.headline)


class StaticContextMixin(object):
    """
    Mixin allows you to set static context through a static property on
    the class.
    """
    static_context = None

    def get_context_data(self, **kwargs):
        kwargs = super(StaticContextMixin, self).get_context_data(**kwargs)

        try:
            kwargs.update(self.get_static_context())
        except (TypeError, ValueError):
            raise ImproperlyConfigured(
                '{0}.static_context must be a dictionary or container '
                'of two-tuples.'.format(self.__class__.__name__))
        else:
            return kwargs

    def get_static_context(self):
        if self.static_context is None:
            raise ImproperlyConfigured(
                '{0} is missing the static_context property. Define '
                '{0}.static_context, or override '
                '{0}.get_static_context()'.format(self.__class__.__name__)
            )
        return self.static_context


class CanonicalSlugDetailMixin(object):
    """
    A mixin that enforces a canonical slug in the url.

    If a urlpattern takes a object's pk and slug as arguments and the slug url
    argument does not equal the object's canonical slug, this mixin will
    redirect to the url containing the canonical slug.
    """
    def dispatch(self, request, *args, **kwargs):
        # Set up since we need to super() later instead of earlier.
        self.request = request
        self.args = args
        self.kwargs = kwargs

        # Get the current object, url slug, and
        # urlpattern name (namespace aware).
        obj = self.get_object()
        slug = self.kwargs.get(self.slug_url_kwarg, None)
        match = resolve(request.path_info)
        url_parts = match.namespaces
        url_parts.append(match.url_name)
        current_urlpattern = ":".join(url_parts)

        # Figure out what the slug is supposed to be.
        if hasattr(obj, "get_canonical_slug"):
            canonical_slug = obj.get_canonical_slug()
        else:
            canonical_slug = self.get_canonical_slug()

        # If there's a discrepancy between the slug in the url and the
        # canonical slug, redirect to the canonical slug.
        if canonical_slug != slug:
            params = {self.pk_url_kwarg: obj.pk,
                      self.slug_url_kwarg: canonical_slug,
                      'permanent': True}
            return redirect(current_urlpattern, **params)

        return super(CanonicalSlugDetailMixin, self).dispatch(
            request, *args, **kwargs)

    def get_canonical_slug(self):
        """
        Override this method to customize what slug should be considered
        canonical.

        Alternatively, define the get_canonical_slug method on this view's
        object class. In that case, this method will never be called.
        """
        return self.get_object().slug


class AllVerbsMixin(object):
    """Call a single method for all HTTP verbs.

    The name of the method should be specified using the class attribute
    ``all_handler``. The default value of this attribute is 'all'.
    """
    all_handler = 'all'

    def dispatch(self, request, *args, **kwargs):
        if not self.all_handler:
            raise ImproperlyConfigured(
                '{0} requires the all_handler attribute to be set.'.format(
                    self.__class__.__name__))

        handler = getattr(self, self.all_handler, self.http_method_not_allowed)
        return handler(request, *args, **kwargs)


class HeaderMixin(object):
    """
    Add arbitrary HTTP headers to a response by specifying them in the
    ``headers`` attribute or by overriding the ``get_headers()`` method.
    """
    headers = {}

    def get_headers(self, request):
        return self.headers

    def dispatch(self, request, *args, **kwargs):
        """
        Override this method to customize the way additional headers are
        retrieved.  It is mandatory that the returned value supports the
        ``.items()`` method.
        """
        response = super(HeaderMixin, self).dispatch(request, *args, **kwargs)
        for key, value in self.get_headers(request).items():
            if key not in response:
                response[key] = value
        return response
