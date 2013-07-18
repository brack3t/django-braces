from django import test
from django.contrib.auth.models import AnonymousUser


class TestViewHelper(object):
    """
    Helper class for unit-testing class based views.
    """
    view_class = None
    request_factory_class = test.RequestFactory

    def setUp(self):
        super(TestViewHelper, self).setUp()
        self.factory = self.request_factory_class()

    def build_request(self, method='GET', path='/test/', user=None, **kwargs):
        """
        Creates a request using request factory.
        """
        fn = getattr(self.factory, method.lower())
        if user is None:
            user = AnonymousUser()

        req = fn(path, **kwargs)
        req.user = user
        return req

    def build_view(self, request, args=None, kwargs=None, view_class=None,
                   **viewkwargs):
        """
        Creates a `view_class` view instance.
        """
        if not args:
            args = ()
        if not kwargs:
            kwargs = {}
        if view_class is None:
            view_class = self.view_class

        return view_class(
            request=request, args=args, kwargs=kwargs, **viewkwargs)

    def dispatch_view(self, request, args=None, kwargs=None, view_class=None,
                      **viewkwargs):
        """
        Creates and dispatches `view_class` view.
        """
        view = self.build_view(request, args, kwargs, view_class, **viewkwargs)
        return view.dispatch(request, *view.args, **view.kwargs)
