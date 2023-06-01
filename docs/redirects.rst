Redirection Mixins
==================

The mixins in this group are all related to redirecting HTTP requests.


.. _RedirectMixin:

RedirectMixin
-------------

.. versionadded:: 2.0

This mixin provides a ``redirect()`` method that can be used to redirect
the request to another URL. You can provide the URL via the ``redirect_url``
attribute or by overriding the ``get_redirect_url()`` method.

::

    from django.views.generic import View

    from braces.mixins import RedirectMixin

    class GetOverHereView(RedirectMixin, View):
        redirect_url = "/scorpion/"

        def get(self, request, *args, **kwargs):
            return self.redirect()

.. _CanonicalRedirectMixin:

CanonicalRedirectMixin
----------------------

This mixin provides a mechanism for ensuring that your URLs are canonical.
It will redirect any non-canonical requests to the correct canonical URL.

::

    class ArticleView(CanonicalRedirectMixin, DetailView):
        model = Article
        canonical_redirect = True

        def get_canonical_url(self):
            return self.get_object().get_absolute_url()

Now, given an ``Article`` object with ``{pk: 1, slug: 'hello-world'}``,
the URL `/articles/1-goodbye-moon` will redirect to
`/articles/1-hello-world` with the HTTP status code
`301 Moved Permanently`. Any other non-canonical slug, not just
`"goodbye-moon"`, will trigger the redirect as well.

You will have to provide your own implementation of `get_canonical_url`
since there is no way to know what the canonical URL is for your object.


.. _RedirectOnFailureMixin:

RedirectOnFailureMixin
----------------------

.. versionadded:: 2.0

This mixin is similar to the :ref:``RedirectMixin`` except that it is
already primed with a ``redirect_url`` that is used when the view's
``dispatch_test`` method returns ``False``. This mixin's
``handle_test_failure`` will redirect to the URL provided.
Alternatively, an exception may be raised by setting ``raise_exception``
to ``True``.

::

    from django.views.generic import View

    from braces.mixins import RedirectOnFailureMixin


    class FailureView(RedirectOnFailureMixin, View):
        redirect_url = "/scorpion/"
        raise_exception = True

        def dispatch_test(self, request, *args, **kwargs):
            return False

.. RedirectToLoginMixin:

RedirectToLoginMixin
--------------------

.. versionadded:: 2.0

This mixin is similar to the :ref:``RedirectMixin`` except that it is
already configured to redirect a user to the ``LOGIN_URL`` in your
settings. You can override these options by specifying ``login_url`` or
overriding ``get_login_url()``.

::

    from django.views.generic import View

    from braces.mixins import RedirectToLoginMixin


    class ShouldIStayOrShouldIGoView(RedirectToLoginMixin, View):
        def dispatch_test(self, request, *args, **kwargs):
            return False
