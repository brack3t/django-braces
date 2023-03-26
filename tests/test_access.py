from datetime import timedelta

import pytest
from django.contrib.auth.models import Group, Permission
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now

pytestmark = [pytest.mark.mixin_view, pytest.mark.mixin_class]


@pytest.mark.mixin_class("PassesTest")
class TestRequestPassesTest:
    def test_success(self, mixin_view, rf):
        req = rf.get("/")
        mixin_view.test_method = lambda x: True
        mixin_view.setup(req)
        response = mixin_view.as_view()(req)
        assert response.status_code == 200

    def test_failure(self, mixin_view):
        mixin_view.test_method = lambda x: False

        with pytest.raises(PermissionDenied):
            mixin_view.as_view()(rf.get("/"))

    def test_non_callable(self, mixin_view):
        mixin_view.request_test = "not_callable"

        with pytest.raises(ImproperlyConfigured):
            mixin_view.as_view()(rf.get("/"))

    def test_missing_method(self, mixin_view):
        with pytest.raises(ImproperlyConfigured):
            mixin_view.as_view()(rf.get("/"))

    def test_none(self, mixin_view):
        mixin_view.request_test = None

        with pytest.raises(ImproperlyConfigured):
            mixin_view.as_view()(rf.get("/"))


@pytest.mark.mixin_class("SuperuserRequiredMixin")
@pytest.mark.django_db
class TestSuperuserRequired:
    def test_success(self, mixin_view, admin_user, rf):
        request = rf.get("/")
        request.user = admin_user
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_superuser(self, mixin_view, django_user_model, rf):
        user = django_user_model.objects.create_user("test", "Test1234")
        request = rf.get("/")
        request.user = user
        response = mixin_view.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin_class("StaffUserRequiredMixin")
@pytest.mark.django_db
class TestStaffUserRequired:
    @pytest.fixture(scope="function")
    @pytest.mark.django_db
    def user(self, django_user_model):
        u = django_user_model.objects.create_user("test", "Test1234")
        yield u
        u.delete()

    def test_success(self, user, mixin_view, rf):
        request = rf.get("/")
        user.is_staff = True
        request.user = user
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_staff(self, user, mixin_view, rf):
        request = rf.get("/")
        request.user = user
        response = mixin_view.as_view()(request)
        assert response.status_code == 302


@pytest.mark.django_db
@pytest.mark.mixin_class("GroupRequiredMixin")
class TestGroupRequired:
    @pytest.fixture(scope="function")
    def group_view(mixin_view):
        mixin_view.group_required = "test"
        yield mixin_view
        del mixin_view

    @pytest.mark.django_db
    @pytest.fixture
    def group(self):
        g = Group.objects.create(name="test")
        yield g
        g.delete()

    @pytest.mark.django_db
    @pytest.fixture
    def user(self, group, django_user_model):
        u = django_user_model.objects.create_user("test", "Test1234")
        u.groups.add(group)
        yield u
        u.delete()

    @pytest.mark.django_db
    def test_success(self, group_view, rf, user):
        request = rf.get("/")
        request.user = user
        response = group_view.as_view()(request)
        assert response.status_code == 200

    @pytest.mark.django_db
    def test_failure(self, group_view, rf, user):
        request = rf.get("/")
        request.user = user
        request.user.groups.clear()
        response = group_view.as_view()(request)
        assert response.status_code == 302

    @pytest.mark.django_db
    def test_no_group_required(self, mixin_view, rf, user):
        mixin_view.group_required = None
        user.groups.clear()

        request = rf.get("/")
        request.user = user

        with pytest.raises(ImproperlyConfigured):
            mixin_view.as_view()(request)

    def test_group_string(self, mixin_view):
        mixin_view.group_required = "test"
        assert mixin_view().get_group_required() == ["test"]


@pytest.mark.mixin_class("AnonymousRequiredMixin")
class TestAnonymousRequired:
    def test_success(self, mixin_view, rf):
        response = mixin_view.as_view()(rf.get("/"))
        assert response.status_code == 200

    def test_authenticated(self, admin_user, mixin_view, rf):
        request = rf.get("/")
        request.user = admin_user
        response = mixin_view.as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin_class("LoginRequiredMixin")
class TestLoginRequired:
    def test_success(self, admin_user, mixin_view, rf):
        request = rf.get("/")
        request.user = admin_user
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view.as_view()(rf.get("/"))
        assert response.status_code == 302


@pytest.mark.mixin_class("RecentLoginRequiredMixin")
class TestRecentLoginRequired:
    def test_success(self, admin_user, mixin_view, rf):
        request = rf.get("/")
        request.user = admin_user
        request.user.last_login = now() - timedelta(minutes=10)
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, admin_user, rf, mixin_view):
        request = rf.post("/")
        request.user = admin_user
        request.user.last_login = now() - timedelta(days=10)
        response = mixin_view.as_view()(request)
        assert response.status_code == 403


@pytest.mark.mixin_class("PermissionRequiredMixin")
@pytest.mark.django_db
class TestPermissionRequired:
    @pytest.fixture(scope="function")
    def mixin_view(self, mixin_view):
        mixin_view.permission_required = "project.add_article"
        return mixin_view

    @pytest.fixture(scope="function")
    @pytest.mark.django_db
    def user(self, django_user_model):
        p = Permission.objects.get(
            content_type__app_label="project", codename="add_article"
        )
        u = django_user_model.objects.create_user("test", "Test1234")
        u.user_permissions.add(p)

        yield u

        u.delete()

    def test_success(self, mixin_view, rf, user):
        request = rf.get("/")
        request.user = user
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_no_permission(self, mixin_view, rf, user):
        request = rf.get("/")
        request.user = user
        request.user.user_permissions.clear()
        response = mixin_view.as_view()(request)
        assert response.status_code == 302

    def test_optional_permissions(self, mixin_view, rf, user):
        mixin_view.permission_required = {"any": "tests.add_article"}
        request = rf.get("/")
        request.user = user
        request.user.user_permissions.clear()
        response = mixin_view.as_view()(request)
        assert response.status_code == 200


@pytest.mark.mixin_class("SSLRequiredMixin")
class TestSSLRequired:
    def test_success(self, mixin_view, rf):
        request = rf.get("/")
        request.is_secure = lambda: True
        response = mixin_view.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, mixin_view, rf):
        request = rf.get("/")
        request.is_secure = lambda: False
        response = mixin_view.as_view()(request)
        assert response.status_code == 301
