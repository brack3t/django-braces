.. django-braces documentation master file, created by
   sphinx-quickstart on Mon Apr 30 10:31:44 2012.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to django-braces's documentation!
=========================================

You can view the code of our project or fork it and add your own mixins (please, send them back to us), on `Github`_.

.. toctree::
    :maxdepth: 2

    Access Mixins <access>
    Form Mixins <form>
    Other Mixins <other>


JsonRequestResponseMixin
================
    
A mixin that attempts to parse request as JSON.  If request is properly formatted, the json is saved to self.request_json as a Python object.  request_json will be None for imparsible requests.

To catch requests that aren't JSON-formatted, set the class attribute
require_json to True.  
Override the class attribute error_response_dict to customize the default error message.

It extends JSONResponseMixin, so those utilities are available as well.

Note: To allow public access to your view, you'll need to use the csrf_exempt decorator or CsrfExemptMixin.

::
    from django.views.generic import View

    from braces.views import CsrfExemptMixin, JsonRequestResponseMixin

    class SomeView(CsrfExemptMixin, JsonRequestResponseMixin):
        require_json = True

        def post(self, request, *args, **kwargs):
            try:
                burrito = self.request_json['burrito']
                toppings = self.request_json['toppings']
            except:
                error_dict = {'message':
                    'your order must include a burrito AND toppings'}
                return self.render_bad_request_response(error_dict)
            place_order(burrito, toppings)
            return self.render_json_response(
                {'message': 'Your order has been placed!'})


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


.. _Github: https://github.com/brack3t/django-braces
