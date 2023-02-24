import pytest
from django.http import HttpResponse
from django.contrib import messages
from django.contrib.messages.middleware import MessageMiddleware
from django.core.exceptions import ImproperlyConfigured
from django.views.generic import View
from django.test import RequestFactory

from braces import mixins


class TestMessages:
    middleware = MessageMiddleware("")

    class _View(mixins.MessagesMixin, View):
        def get(self, request):
            self.messages.add_message(messages.INFO, "test")
            return HttpResponse("OK")

    def get_request(self):
        request = RequestFactory().get("/")
        self.middleware.process_request(request)
        return request

    def get_response(self, request, view=_View):
        _view = view.as_view()
        response = _view(request)
        self.middleware.process_response(request, response)
        return response

    def get_request_response(self, view):
        request = self.get_request()
        response = self.get_response(request, view)
        return request, response

    @pytest.mark.parametrize(
        "level",
        [
            messages.DEBUG,
            messages.INFO,
            messages.SUCCESS,
            messages.WARNING,
            messages.ERROR,
        ],
    )
    def test_add_messages(self, level):
        class _View(self._View):
            def get(self, request):
                self.messages.add_message(level, "test")
                return HttpResponse("OK")

        req, _ = self.get_request_response(_View)
        _messages = list(req._messages)
        assert len(_messages) == 1
        assert _messages[0].message == "test"
        assert _messages[0].level == level


class TestFormValidMessage:
    middleware = MessageMiddleware("")

    class _View(mixins.FormValidMessageMixin, View):
        form_valid_message = "valid"

        def get(self, request):
            return HttpResponse("OK")

    def test_form_valid_message(self):
        assert self._View().get_form_valid_message() == "valid"

    def test_form_valid_message_none(self):
        class _View(self._View):
            form_valid_message = None

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_valid_message()

    def test_form_valid_message_not_str(self):
        class _View(self._View):
            form_valid_message = 1

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_valid_message()

    def test_form_valid_message_not_set(self):
        class _View(mixins.FormValidMessageMixin, View):
            def get(self, request):
                return HttpResponse("OK")

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_valid_message()


class TestFormInvalidMessage:
    middleware = MessageMiddleware("")

    class _View(mixins.FormInvalidMessageMixin, View):
        form_invalid_message = "invalid"

        def get(self, request):
            return HttpResponse("OK")

    def get_request(self):
        request = RequestFactory().get("/")
        self.middleware.process_request(request)
        return request

    def get_response(self, request, view=_View):
        _view = view.as_view()
        response = _view(request)
        self.middleware.process_response(request, response)
        return response

    def get_request_response(self, view):
        request = self.get_request()
        response = self.get_response(request, view)
        return request, response

    def test_form_invalid_message(self):
        _view = self._View()
        assert _view.get_form_invalid_message() == "invalid"

    def test_form_invalid_message_none(self):
        class _View(self._View):
            form_invalid_message = None

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_invalid_message()

    def test_form_invalid_message_not_str(self):
        class _View(self._View):
            form_invalid_message = 1

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_invalid_message()

    def test_form_valid_message_not_set(self):
        class _View(mixins.FormInvalidMessageMixin, View):
            def get(self, request):
                return HttpResponse("OK")

        with pytest.raises(ImproperlyConfigured):
            _View().get_form_invalid_message()
