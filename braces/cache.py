class NeverCacheMixin(object):
    """
    Mixin that makes sure View is never cached.
    """
    @method_decorator(never_cache)
    def dispatch(self, *args, **kwargs):
        return super(NeverCacheMixin, self).dispatch(*args, **kwargs)

class CacheMixin(object):
    """
    Mixin that allows caching for the view it is applied to.
    View is being cached for `ttl`s, default is 60 seconds.
    """

    ttl = 60
 
    def get_ttl(self):
        if self.ttl is None:
            raise ImproperlyConfigured("%(cls)s does not have a value for `ttl`. ")
        return self.ttl
  
    def dispatch(self, *args, **kwargs):
        return cache_page(self.get_ttl())(super(CacheMixin, self).dispatch)(*args, **kwargs)

