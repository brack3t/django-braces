from django.views.generic import FormView, UpdateView, View
from django.views.generic.detail import SingleObjectMixin
from django.http import HttpResponseRedirect, HttpResponse
from django.core.urlresolvers import reverse
from django.views.decorators.csrf import csrf_exempt
from django.utils.decorators import method_decorator
import json

from braces.views import JsonRequestResponseMixin, CsrfExemptMixin

class MainPage(CsrfExemptMixin, JsonRequestResponseMixin, View):

    def get(self, request, *args, **kwargs):
        return self.render_json_response({'message': 'Thanks!'})

    def post(self, request, *args, **kwargs):
        if self.request_json is None:
            # This method will return a HTTP400 error:
            return self.render_bad_request_response()
        return self.render_json_response(
            {'message': 'Thanks!'})