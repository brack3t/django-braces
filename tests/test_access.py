from django.http import HttpResponse
from django.test import RequestFactory
from django.views import View
from braces.mixins import RequestPassesTest, SuperuserRequiredMixin


class TestRequestPassesTest:
    def test_success(self):
        class TestView(RequestPassesTest, View):
            request_test = 'test_method'
            def test_method(self):
                return True
            def get(self, request):
                return HttpResponse('OK')
        response = TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 200


class TestSuperuserRequired:
    class TestView(SuperuserRequiredMixin, View):
        def get(self, request):
            return HttpResponse('OK')

    def test_success(self, admin_user):
        request = RequestFactory().get('/')
        request.user = admin_user
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self.TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 302

    def test_non_superuser(self, django_user_model):
        user = django_user_model.objects.create_user("test", "Test1234")
        request = RequestFactory().get('/')
        request.user = user
        response = self.TestView.as_view()(request)
        assert response.status_code == 302