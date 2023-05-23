JSON Mixins
===========

This is a collection of mixins for working with JSON objects and Django
responses.

.. _JSONResponseMixin:

JSONResponseMixin
-----------------

This mixin provides a way to render JSON responses. It's a thin wrapper
around Django's ``JsonResponse``. It should **not** be used for complex
serialization of objects. For that, use Django REST Framework or your
favorite serialization library.

::

    from django.views.generic import View
    from django_json_mixins.views import JSONResponseMixin

    class MyView(JSONResponseMixin, View):
        def get(self, request, *args, **kwargs):
            return self.render_json_response({'foo': 'bar'})

In addition to ``render_json_response``, this mixin also provides
a ``context`` attribute which can be used to store static values
for the rendered JSON.


.. _JSONRequestMixin:

JSONRequestMixin
----------------

This mixin allows your views to respond differently to requests made
asynchronously, usually via JavaScript/AJAX. If the request is made
with the ``X-Requested-With`` header set to ``XMLHttpRequest``, the
view will handle that request with a ``*_json`` method instead of the
normal ``get``, ``post``, or other method.

::

    from django.views.generic import View
    from django.http import JsonResponse
    from braces.mixins import JSONRequestMixin

    class MyView(JSONRequestMixin, View):
        def get_json(self, request, *args, **kwargs) -> JsonResponse:
            return self.get_object().serialized()

        def get(self, request, *args, **kwargs):
            return self.render_to_response({"object": self.get_object()})
