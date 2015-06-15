from calendar import timegm
from django.core.exceptions import ImproperlyConfigured
from django.core.urlresolvers import resolve
from django.shortcuts import redirect
from django.utils.cache import patch_response_headers, patch_vary_headers, patch_cache_control
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

    # If True, shared caches should not cache
    private = True

    # Do not cache at all
    no_cache = False

    # Tell shared caches not to transform (file format etc)
    no_transform = False

    # Always revalidate, do not serve stale content
    must_revalidate = False

    # Shared caches must always revalidate, do not serve stale content
    proxy_revalidate = False

    # Maximum age (in seconds) this resource may be cached for
    max_age = 3600

    # Maximum age (in seconds) this resource may be cached for
    # by shared caches (defaults to max_age)
    s_maxage = None

    # Maximum age (in seconds) this resource may be cached for (HTTP/1.0)
    # (defaults to max_age)
    cache_timeout = None

    # Vary the cache on the following headers
    cache_varies = ['Accept']

    # HTTP/1.1 Caching (Vary, Etag, Cache-Control)
    def get_cache_varies(self):
        return self.cache_varies

    def get_etag(self):
        return None

    def get_last_modified(self):
        """Return an aware datetime or None"""
        return None

    # Cache-Control value accessors
    def get_private(self):
        return self.private

    def get_public(self):
        if self.private is None:
            return None
        return not self.private

    def get_no_cache(self):
        return self.no_cache

    def get_no_transform(self):
        return self.no_transform

    def get_must_revalidate(self):
        return self.must_revalidate

    def get_proxy_revalidate(self):
        return self.proxy_revalidate

    def get_max_age(self):
        return self.max_age

    def get_s_maxage(self):
        if self.s_maxage is None:
            return self.get_max_age()
        else:
            return self.s_maxage

    def get_cache_control_kwargs(self):
        kwargs = dict(
            private=self.get_private() or None,
            public=self.get_public() or None,
            no_cache=self.get_no_cache() or None,
            no_transform=self.get_no_transform() or None,
            must_revalidate=self.get_must_revalidate() or None,
            proxy_revalidate=self.get_proxy_revalidate() or None,
            max_age=self.get_max_age(),
            s_maxage=self.get_s_maxage(),
        )
        # Assume a None values indicate the kwarg should be ignored
        kwargs = {k: v for k, v in kwargs.items() if v is not None}
        return kwargs

    # HTTP/1.1 headers
    def get_cache_timeout(self):
        if self.cache_timeout is None:
            return self.get_max_age()
        return self.cache_timeout

    def is_cacheable(self, request, response):
        """Should cache headers be sent at all"""
        return (request.method in ['GET', 'HEAD', 'PUT'] and
                response.status_code in [200, 203, 206, 410])

    # Accessors now finished. Now let's get on with it...

    def dispatch(self, request, *args, **kwargs):
        response = super(HttpCacheMixin, self).dispatch(request, *args, **kwargs)
        if self.is_cacheable(request, response):
            self._set_last_modified(request, response)
            self._set_etag(request, response)
            self._set_cache_control(request, response)
            self._set_varies(request, response)
            self._django_mop_up(request, response)
        return response

    def _set_last_modified(self, request, response):
        last_modified = self.get_last_modified()
        if last_modified is not None:
            # Convert the last_modified datetime to a UTC timestamp
            # (as this is what Django's http_date() expects)
            utc_last_modified = make_naive(last_modified, timezone=UTC())
            utc_timestamp = timegm(utc_last_modified.timetuple())
            response['Last-Modified'] = http_date(utc_timestamp)

    def _set_etag(self, request, response):
            etag = self.get_etag()
            if etag is not None:
                response['ETag'] = etag

    def _set_cache_control(self, request, response):
        patch_cache_control(response, **self.get_cache_control_kwargs())

    def _set_varies(self, request, response):
        cache_varies = self.get_cache_varies()
        if cache_varies:
            patch_vary_headers(response, cache_varies)

    def _django_mop_up(self, request, response):
        """Apply Django's caching logic

        This will use the settings CACHE_MIDDLEWARE_SECONDS & USE_ETAGS
        to set the Last-Modifed, Expires, and ETags if not set by this view
        """
        cache_timeout = self.get_cache_timeout()
        if cache_timeout is not None:
            patch_response_headers(response, int(cache_timeout))
