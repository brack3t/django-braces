from calendar import timegm
from django.core.exceptions import ImproperlyConfigured
from django.core.urlresolvers import resolve
from django.shortcuts import redirect
from django.utils.cache import patch_response_headers, patch_vary_headers
from django.utils.encoding import force_text
from django.utils.http import http_date
from django.utils.timezone import UTC, make_naive


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
        return force_text(self.headline)


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
        
class HttpCacheMixin(object):
    """
    A mixin that provides HTTP cache management that works exactly like the
    @cache_control and @vary_on_headers decorators.
    """
    cache_timeout = 60
    cache_varies = ['Accept']

    def get_cache_timeout(self):
        return self.cache_timeout

    def get_cache_varies(self):
        return self.cache_varies

    def get_last_modified(self):
        """
        Return an aware datetime or None
        """
        return None

    def get_etag(self):
        return None

    @classmethod
    def cacheable(cls, request, response):
        return (request.method in ['GET', 'HEAD', 'PUT'] and
                response.status_code in [200, 203, 206, 410])

    def dispatch(self, request, *args, **kwargs):
        response = super(HttpCacheMixin, self).dispatch(request, *args, **kwargs)
        if self.cacheable(request, response):
            last_modified = self.get_last_modified()
            if last_modified is not None:
                # Convert the last_modified datetime to a UTC timestamp
                # (as this is what Django's http_date() expects)
                utc_last_modified = make_naive(last_modified, timezone=UTC())
                utc_timestamp = timegm(utc_last_modified.timetuple())
                response['Last-Modified'] = http_date(utc_timestamp)
            etag = self.get_etag()
            if etag is not None:
                response['ETag'] = etag
            cache_timeout = int(self.get_cache_timeout())
            patch_response_headers(response, cache_timeout)
            cache_varies = self.get_cache_varies()
            if len(cache_varies):
                patch_vary_headers(response, cache_varies)
        return response
