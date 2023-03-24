from datetime import timedelta

import pytest
from django.contrib.auth.models import Group, Permission
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now

pytestmark = [pytest.mark.view_class, pytest.mark.mixin_class]


@pytest.mark.mixin_class("PassesTest")
class TestRequestPassesTest:
    def test_success(self, view_class, rf):
        view_class.test_method = lambda x: True
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 200

    def test_failure(self, view_class):
        view_class.test_method = lambda x: False

        with pytest.raises(PermissionDenied):
            view_class.as_view()(rf.get("/"))

    def test_non_callable(self, view_class):
        view_class.request_test = "not_callable"

        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(rf.get("/"))

    def test_missing_method(self, view_class):
        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(rf.get("/"))

    def test_none(self, view_class):
        view_class.request_test = None

        with pytest.raises(ImproperlyConfigured):
            view_class.as_view()(rf.get("/"))


@pytest.mark.mixin_class("SuperuserRequiredMixin")
@pytest.mark.django_db
class TestSuperuserRequired:
    def test_success(self, view_class, admin_user, rf):
        request = rf.get("/")
        request.user = admin_user
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, view_class):
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_superuser(self, view_class, django_user_model, rf):
        user = django_user_model.objects.create_user("test", "Test1234")
        request = rf.get("/")
        request.user = user
        response = view_class.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin_class("StaffUserRequiredMixin")
@pytest.mark.django_db
class TestStaffUserRequired:
    @pytest.fixture(scope="class")
    @pytest.mark.django_db
    def user(self, django_user_model):
        u = django_user_model.objects.create_user("test", "Test1234")
        yield u
        u.delete()

    def test_success(self, user, view_class, rf):
        request = rf.get("/")
        user.is_staff = True
        request.user = user
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, view_class, rf):
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_staff(self, user, view_class, rf):
        request = rf.get("/")
        request.user = user
        response = view_class.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin_class("GroupRequiredMixin")
@pytest.mark.django_db
class TestGroupRequired:
    @pytest.fixture
    def group_view_class(view_class):
        view_class.group_required = ["test", "fake"]
        return view_class

    @pytest.mark.django_db
    @pytest.fixture
    def user(self, group, django_user_model):
        u = django_user_model.objects.create_user("test", "Test1234")
        u.groups.add(group)
        yield u
        u.delete()

    @pytest.fixture
    @pytest.mark.django_db
    def group(self):
        g = Group.objects.create(name="test")
        yield g
        g.delete()

    @pytest.mark.django_db
    def test_success(self, group_view_class, rf, user):
        request = rf.get("/")
        request.user = user
        response = group_view_class.as_view()(request)
        assert response.status_code == 200

    @pytest.mark.django_db
    def test_failure(self, group_view_class, rf, user):
        request = rf.get("/")
        request.user = user
        request.user.groups.clear()
        response = group_view_class.as_view()(request)
        assert response.status_code == 302

    @pytest.mark.django_db
    def test_no_group_required(self, group_view_class, rf, user):
        group_view_class.group_required = None
        user.groups.clear()

        request = rf.get("/")
        request.user = user

        with pytest.raises(ImproperlyConfigured):
            group_view_class.as_view()(request)

    def test_group_string(self, group_view_class):
        group_view_class.group_required = "test"
        assert group_view_class.get_group_required() == ["test"]


@pytest.mark.mixin_class("AnonymousRequiredMixin")
class TestAnonymousRequired:
    def test_success(self, view_class, rf):
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 200

    def test_authenticated(self, admin_user, view_class, rf):
        request = rf.get("/")
        request.user = admin_user
        response = view_class.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin_class("LoginRequiredMixin")
class TestLoginRequired:
    def test_success(self, admin_user, view_class, rf):
        request = rf.get("/")
        request.user = admin_user
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, view_class, rf):
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 302


@pytest.mark.mixin_class("RecentLoginRequiredMixin")
class TestRecentLoginRequired:
    def test_success(self, admin_user, view_class, rf):
        request = rf.get("/")
        request.user = admin_user
        request.user.last_login = now() - timedelta(minutes=10)
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, admin_user, rf, view_class):
        request = rf.post("/")
        request.user = admin_user
        request.user.last_login = now() - timedelta(days=10)
        response = view_class.as_view()(request)
        assert response.status_code == 403


@pytest.mark.mixin_class("PermissionRequiredMixin")
@pytest.mark.django_db
class TestPermissionRequired:
    @pytest.fixture(scope="class")
    def view_class(self, view_class):
        view_class.permission_required = "project.add_article"
        return view_class.copy()

    @pytest.fixture(scope="class")
    @pytest.mark.django_db
    def user(self, django_user_model):
        p = Permission.objects.get(
            content_type__app_label="project", codename="add_article"
        )
        u = django_user_model.objects.create_user("test", "Test1234")
        u.user_permissions.add(p)

        yield u

        u.delete()

    def test_success(self, view_class, rf, user):
        request = rf.get("/")
        request.user = user
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, view_class, rf):
        response = view_class.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_no_permission(self, view_class, rf, user):
        request = rf.get("/")
        request.user = user
        request.user.user_permissions.clear()
        response = view_class.as_view()(request)
        assert response.status_code == 302

    def test_optional_permissions(self, view_class, rf, user):
        view_class.permission_required = {"any": "tests.add_article"}
        request = rf.get("/")
        request.user = user
        request.user.user_permissions.clear()
        response = view_class.as_view()(request)
        assert response.status_code == 200


@pytest.mark.mixin_class("SSLRequiredMixin")
class TestSSLRequired:
    def test_success(self, view_class, rf):
        request = rf.get("/")
        request.is_secure = lambda: True
        response = view_class.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, view_class, rf):
        request = rf.get("/")
        request.is_secure = lambda: False
        response = view_class.as_view()(request)
        assert response.status_code == 301
