Django REST Framework Mixins
============================

These mixins relate to the Django REST Framework (DRF). They're made
to help with DRF development.

.. _MultipleSerializersMixin:

MultipleSerializerMixin
-----------------------

This mixin is similar to :ref:`MultipleFormsMixin` but handles DRF
serializers instead of Django forms.

To use the mixin, you need to define a ``serializers`` attribute on
your view class. This attribute should be a dictionary mapping HTTP
verbs with serializer classes.

::

    class CerealView(MultipleSerializerMixin, generics.GenericAPIView):
        serializers = {
            'GET': MyGetSerializer,
            'POST': MyPostSerializer,
        }

        def get(self, request, *args, **kwargs):
            # self.serializer_class will be MyGetSerializer
            ...

        def post(self, request, *args, **kwargs):
            # self.serializer_class will be MyPostSerializer
            ...
