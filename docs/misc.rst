Miscellaneous Mixins
====================

These mixins don't have a better home at this point.

.. contents::

.. _StaticContextMixin:

StaticContextMixin
------------------

The ``StaticContextMixin`` allows you to easily set static context data
by using the ``static_context`` attribute.

.. note::
    While it's possible to override the ``StaticContextMixin.get_static_context method``,
    it's not the best option. If you have a need to override a method for
    dynamic context data, it's best to override the standard ``get_context_data``
    method of Django's generic class-based views.

::

    # views.py

    from django.views import TemplateView

    from braces.mixins import StaticContextMixin


    class ContextTemplateView(StaticContextMixin, TemplateView):
        static_context = {"custom_logo": "/static/images/logo.png"}
