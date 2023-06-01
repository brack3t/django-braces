Message Mixins
==============

These mixins relate to sending temporary/flash messages to users.

.. _MessagesMixin:

MessagesMixin
-------------

.. versionadded:: 2.0

This mixin adds a new ``messages`` attribute to the view. This attribute
is a wrapper around Django's ``django.contrib.messages`` framework and
will automatically include the ``request``, as well.

::

    from django.views.generic import TemplateView

    from braces.mixins import MessagesMixin


    class DebugView(MessageMixin, TemplateView):
        """
        This view will add a debug message which can then be displayed
        in the template.
        """
        template_name = "my_template.html"

        def get(self, request, *args, **kwargs):
            self.messages.debug("This is a debug message.")
            return super(MyView, self).get(request, *args, **kwargs)


.. _FormValidMessageMixin:

FormValidMessageMixin
---------------------

This mixin adds a ``form_valid_message`` attribute to the view. This
attribute can be used to specify a message that will be sent to the
next request where you can display it to a user. The message is only sent
if the form is valid.

::

    from django.views.generic import CreateView

    from braces.mixins import FormValidMessageMixin

    class CreateRecord(FormValidMessageMixin, CreateView):
        form_valid_message = "Great job creating that record!"
        ...


.. _FormInvalidMessageMixin:

FormInvalidMessageMixin
-----------------------

This mixin adds a ``form_invalid_message`` attribute to the view. This
attribute can be used to specify a message that will be sent to the
next request where you can display it to a user. The message is only sent
if the form is invalid.

::

    from django.views.generic import CreateView

    from braces.mixins import FormInvalidMessageMixin

    class CreateRecord(FormInvalidMessageMixin, CreateView):
        form_invalid_message = "Oh no, something went wrong!"
        ...


.. _FormMessagesMixin:

FormMessagesMixin
-----------------

This mixin is a combination of ``FormValidMessageMixin`` and
``FormInvalidMessageMixin``. It adds both ``form_valid_message`` and
``form_invalid_message`` attributes to the view.

::

    from django.views.generic import CreateView

    from braces.mixins import FormMessagesMixin

    class CreateRecord(FormMessagesMixin, CreateView):
        form_valid_message = "Great job creating that record!"
        form_invalid_message = "Oh no, something went wrong!"
        ...
