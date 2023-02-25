import copy
from datetime import timedelta
from importlib import import_module

import pytest
from django.conf import settings
from django.contrib.auth import get_user_model
from django.contrib.auth.models import Group, Permission
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.http import HttpResponse
from django.test import RequestFactory
from django.utils.timezone import now
from django.views import View

from braces import mixins


class TestRequestPassesTest:
    class _View(mixins.PassesTest, View):
        request_test = "test_method"

        def get(self, request):
            return HttpResponse("OK")

    def test_success(self):
        view_class = copy.copy(self._View)
        view_class.test_method = lambda x: True
        response = view_class.as_view()(RequestFactory().get("/"))
        assert response.status_code == 200

    def test_failure(self):
        view_class = copy.copy(self._View)
        view_class.test_method = lambda x: False

        with pytest.raises(PermissionDenied):
            view_class.as_view()(RequestFactory().get("/"))

    def test_non_callable(self):
        view_class = copy.copy(self._View)
        view_class.request_test = "not_callable"

        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(RequestFactory().get("/"))

    def test_missing_method(self):
        with pytest.raises(ImproperlyConfigured):
            self._View.as_view()(RequestFactory().get("/"))

    def test_none(self):
        view_class = copy.copy(self._View)
        view_class.request_test = None

        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(RequestFactory().get("/"))


@pytest.mark.django_db
class TestSuperuserRequired:
    class _View(mixins.SuperuserRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def test_success(self, admin_user):
        request = RequestFactory().get("/")
        request.user = admin_user
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self._View.as_view()(RequestFactory().get("/"))
        assert response.status_code == 302

    def test_non_superuser(self):
        user = get_user_model().objects.create_user("test", "Test1234")
        request = RequestFactory().get("/")
        request.user = user
        response = self._View.as_view()(request)
        assert response.status_code == 302


@pytest.mark.django_db
class TestStaffUserRequired:
    class _View(mixins.StaffUserRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")

    def test_success(self):
        request = RequestFactory().get("/")
        self.user.is_staff = True
        request.user = self.user
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self._View.as_view()(RequestFactory().get("/"))
        assert response.status_code == 302

    def test_non_staff(self):
        request = RequestFactory().get("/")
        request.user = self.user
        response = self._View.as_view()(request)
        assert response.status_code == 302


@pytest.mark.django_db
class TestGroupRequired:
    class _View(mixins.GroupRequiredMixin, View):
        group_required = ["test", "fake"]

        def get(self, request):
            return HttpResponse("OK")

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")
        test_group = Group.objects.create(name="test")
        self.user.groups.add(test_group)

    def test_success(self):
        request = RequestFactory().get("/")
        request.user = self.user
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_failure(self):
        request = RequestFactory().get("/")
        request.user = self.user
        self.user.groups.clear()
        response = self._View.as_view()(request)
        assert response.status_code == 302

    def test_no_group_required(self):
        view_class = copy.copy(self._View)
        view_class.group_required = None
        request = RequestFactory().get("/")
        request.user = self.user
        self.user.groups.clear()
        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(request)

    def test_group_string(self):
        view_class = copy.copy(self._View)
        view_class.group_required = "test"
        assert self._View().get_group_required() == ["test"]


class TestAnonymousRequired:
    class _View(mixins.AnonymousRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def test_success(self):
        response = self._View.as_view()(RequestFactory().get("/"))
        assert response.status_code == 200

    def test_authenticated(self, admin_user):
        request = RequestFactory().get("/")
        request.user = admin_user
        response = self._View.as_view()(request)
        assert response.status_code == 302


class TestLoginRequired:
    class _View(mixins.LoginRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def test_success(self, admin_user):
        request = RequestFactory().get("/")
        request.user = admin_user
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self._View.as_view()(RequestFactory().get("/"))
        assert response.status_code == 302


class TestRecentLoginRequired:
    class _View(mixins.RecentLoginRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def setup_method(self):
        engine = import_module(settings.SESSION_ENGINE)
        self.session = engine.SessionStore()

    def test_success(self, admin_user):
        request = RequestFactory().get("/")
        request.user = admin_user
        request.user.last_login = now() - timedelta(minutes=10)
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, admin_user):
        request = RequestFactory().post("/")
        request.user = admin_user
        # request.session = self.session
        request.user.last_login = now() - timedelta(days=10)
        response = self._View.as_view()(request)
        assert response.status_code == 403


@pytest.mark.django_db
class TestPermissionRequired:
    class _View(mixins.PermissionRequiredMixin, View):
        permission_required = "project.add_article"

        def get(self, request):
            return HttpResponse("OK")

    def setup_method(self):
        self.user = get_user_model().objects.create_user("test", "Test1234")
        self.permission = Permission.objects.get(
            content_type__app_label="project", codename="add_article"
        )
        self.user.user_permissions.add(self.permission)

    def test_success(self):
        request = RequestFactory().get("/")
        request.user = self.user
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self):
        response = self._View.as_view()(RequestFactory().get("/"))
        assert response.status_code == 302

    def test_no_permission(self):
        request = RequestFactory().get("/")
        request.user = self.user
        self.user.user_permissions.remove(self.permission)
        response = self._View.as_view()(request)
        assert response.status_code == 302

    def test_optional_permissions(self):
        view_class = copy.copy(self._View)
        view_class.permission_required = {"any": "tests.add_article"}
        request = RequestFactory().get("/")
        request.user = self.user
        self.user.user_permissions.remove(self.permission)
        response = view_class.as_view()(request)
        assert response.status_code == 200


class TestSSLRequired:
    class _View(mixins.SSLRequiredMixin, View):
        def get(self, request):
            return HttpResponse("OK")

    def test_success(self):
        request = RequestFactory().get("/")
        request.is_secure = lambda: True
        response = self._View.as_view()(request)
        assert response.status_code == 200

    def test_failure(self):
        request = RequestFactory().get("/")
        request.is_secure = lambda: False
        response = self._View.as_view()(request)
        assert response.status_code == 301
