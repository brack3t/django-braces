from importlib import import_module
import pytest
from django.conf import settings
from django.contrib.auth import get_user_model, REDIRECT_FIELD_NAME
from django.contrib.auth.models import Group, Permission
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.http import HttpResponse
from django.test import RequestFactory
from django.utils.timezone import now
from django.views import View
from braces import mixins
from datetime import timedelta
from unittest import mock


class TestRequestPassesTest:
    def test_success(self):
        class TestView(mixins.RequestPassesTest, View):
            request_test = 'test_method'
            def test_method(self):
                return True
            def get(self, request):
                return HttpResponse('OK')
        response = TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 200

    def test_non_callable(self):
        class TestView(mixins.RequestPassesTest, View):
            request_test = 'test_method'
            test_method = 'not callable'
            def get(self, request):
                return HttpResponse('OK')
        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(RequestFactory().get('/'))

    def test_missing_method(self):
        class TestView(mixins.RequestPassesTest, View):
            request_test = 'test_method'
            def get(self, request):
                return HttpResponse('OK')
        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(RequestFactory().get('/'))

    def test_none(self):
        class TestView(mixins.RequestPassesTest, View):
            request_test = None
            def get(self, request):
                return HttpResponse('OK')
        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(RequestFactory().get('/'))

    def test_failure(self):
        class TestView(mixins.RequestPassesTest, View):
            request_test = 'test_method'
            def test_method(self):
                return False
            def get(self, request):
                return HttpResponse('OK')
        with pytest.raises(PermissionDenied):
            TestView.as_view()(RequestFactory().get('/'))


class Test_Redirect:
    def test_get_login_url(self):
        class TestView(mixins._Redirect, View):
            login_url = 'test'
            def get(self, request):
                return HttpResponse('OK')
        assert TestView().get_login_url() == 'test'

    def test_get_login_url_default(self):
        class TestView(mixins._Redirect, View):
            def get(self, request):
                return HttpResponse('OK')
        assert TestView().get_login_url() == settings.LOGIN_URL

    def test_get_login_url_missing(self, settings):
        class TestView(mixins._Redirect, View):
            login_url = None
            def get(self, request):
                return HttpResponse('OK')
        del settings.LOGIN_URL
        with pytest.raises(ImproperlyConfigured):
            TestView().get_login_url()

    def test_get_redirect_field_name(self):
        class TestView(mixins._Redirect, View):
            redirect_field_name = 'test'
            def get(self, request):
                return HttpResponse('OK')
        assert TestView().get_redirect_field_name() == 'test'

    def test_get_redirect_field_name_default(self):
        class TestView(mixins._Redirect, View):
            def get(self, request):
                return HttpResponse('OK')
        assert TestView().get_redirect_field_name() == REDIRECT_FIELD_NAME

    def test_get_redirect_field_name_missing(self):
        class TestView(mixins._Redirect, View):
            redirect_field_name = None
            def get(self, request):
                return HttpResponse('OK')
        with pytest.raises(ImproperlyConfigured):
            TestView().get_redirect_field_name()

    @mock.patch('braces.mixins._Redirect.redirect')
    def test_test_failure(self, mock_redirect):
        class TestView(mixins._Redirect, View):
            raise_exception = False
            def get(self, request):
                return HttpResponse('OK')

        TestView().handle_test_failure()
        mock_redirect.assert_called

    @mock.patch('braces.mixins._Redirect.redirect')
    def test_test_anonymous(self, mock_redirect):
        class TestView(mixins._Redirect, View):
            raise_exception = False
            redirect_unauthenticated_users = True
            def get(self, request):
                return HttpResponse('OK')

        TestView().handle_test_failure()
        mock_redirect.called

    @mock.patch('braces.mixins._Redirect.redirect')
    def test_test_exception(self, mock_redirect):
        class TestView(mixins._Redirect, View):
            raise_exception = ImproperlyConfigured
            def get(self, request):
                return HttpResponse('OK')

        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(RequestFactory().get('/'))
        mock_redirect.assert_called_once

    @mock.patch('braces.mixins._Redirect.redirect')
    def test_test_callable(self, mock_redirect):
        class TestView(mixins._Redirect, View):
            raise_exception = "fail"

            def get(self, request):
                return HttpResponse('OK')
            def test_method(self):
                return False
            def fail(self):
                raise ImproperlyConfigured

        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(RequestFactory().get('/'))
        mock_redirect.assert_called_once


@pytest.mark.django_db
class TestSuperuserRequired:
    class TestView(mixins.SuperuserRequiredMixin, View):
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

    def test_non_superuser(self):
        user = get_user_model().objects.create_user("test", "Test1234")
        request = RequestFactory().get('/')
        request.user = user
        response = self.TestView.as_view()(request)
        assert response.status_code == 302


@pytest.mark.django_db
class TestStaffUserRequired:
    class TestView(mixins.StaffUserRequiredMixin, View):
        def get(self, request):
            return HttpResponse('OK')

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")

    def test_success(self):
        request = RequestFactory().get('/')
        self.user.is_staff = True
        request.user = self.user
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self.TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 302

    def test_non_staff(self):
        request = RequestFactory().get('/')
        request.user = self.user
        response = self.TestView.as_view()(request)
        assert response.status_code == 302

@pytest.mark.django_db
class TestGroupRequired:
    class TestView(mixins.GroupRequiredMixin, View):
        group_required = ['test', 'fake']
        def get(self, request):
            return HttpResponse('OK')

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")
        test_group = Group.objects.create(name='test')
        self.user.groups.add(test_group)

    def test_success(self):
        request = RequestFactory().get('/')
        request.user = self.user
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_failure(self):
        request = RequestFactory().get('/')
        request.user = self.user
        self.user.groups.clear()
        response = self.TestView.as_view()(request)
        assert response.status_code == 302

    def test_no_group_required(self):
        class TestView(mixins.GroupRequiredMixin, View):
            group_required = None
            def get(self, request):
                return HttpResponse('OK')

        request = RequestFactory().get('/')
        request.user = self.user
        self.user.groups.clear()
        with pytest.raises(ImproperlyConfigured):
            TestView.as_view()(request)

    def test_group_string(self):
        class TestView(mixins.GroupRequiredMixin, View):
            group_required = 'test'
            def get(self, request):
                return HttpResponse('OK')

        assert TestView().get_group_required() == ['test']


class TestAnonymousRequired:
    class TestView(mixins.AnonymousRequiredMixin, View):
        def get(self, request):
            return HttpResponse('OK')

    def test_success(self):
        response = self.TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 200

    def test_authenticated(self, admin_user):
        request = RequestFactory().get('/')
        request.user = admin_user
        response = self.TestView.as_view()(request)
        assert response.status_code == 302


class TestLoginRequired:
    class TestView(mixins.LoginRequiredMixin, View):
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


class TestRecentLoginRequired:
    class TestView(mixins.RecentLoginRequiredMixin, View):
        def get(self, request):
            return HttpResponse('OK')

    def setup_method(self):
        engine = import_module(settings.SESSION_ENGINE)
        self.session = engine.SessionStore()

    def test_success(self, admin_user):
        request = RequestFactory().get('/')
        request.user = admin_user
        request.user.last_login = now() - timedelta(minutes=10)
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, admin_user):
        request = RequestFactory().get('/')
        request.user = admin_user
        request.session = self.session
        request.user.last_login = now() - timedelta(days=10)
        response = self.TestView.as_view()(request)
        assert response.status_code == 302


@pytest.mark.django_db
class TestPermissionRequired:
    class TestView(mixins.PermissionRequiredMixin, View):
        permission_required = 'tests.add_article'
        def get(self, request):
            return HttpResponse('OK')

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")
        self.permission = Permission.objects.get(
            content_type__app_label="tests", codename="add_article"
        )
        self.user.user_permissions.add(self.permission)

    def test_success(self):
        request = RequestFactory().get('/')
        request.user = self.user
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self.TestView.as_view()(RequestFactory().get('/'))
        assert response.status_code == 302

    def test_no_permission(self):
        request = RequestFactory().get('/')
        request.user = self.user
        self.user.user_permissions.remove(self.permission)
        response = self.TestView.as_view()(request)
        assert response.status_code == 302

    def test_optional_permissions(self):
        class TestView(self.TestView):
            permission_required = {"any": 'tests.add_article'}

        request = RequestFactory().get('/')
        request.user = self.user
        self.user.user_permissions.remove(self.permission)
        response = TestView.as_view()(request)
        assert response.status_code == 200


class TestSSLRequired:
    class TestView(mixins.SSLRequiredMixin, View):
        def get(self, request):
            return HttpResponse('OK')

    def test_success(self):
        request = RequestFactory().get('/')
        request.is_secure = lambda: True
        response = self.TestView.as_view()(request)
        assert response.status_code == 200

    def test_failure(self):
        request = RequestFactory().get('/')
        request.is_secure = lambda: False
        response = self.TestView.as_view()(request)
        assert response.status_code == 301