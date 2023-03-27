from datetime import timedelta

import pytest
from django.contrib.auth.models import Group, Permission
from django.contrib.sessions.backends.db import SessionStore
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now

pytestmark = [pytest.mark.mixin_view]


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
class TestGroupRequired:
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
        u.refresh_from_db()
        assert u.groups.count() == 1
        yield u
        u.delete()

    @pytest.fixture
    def default_view(self, mixin_view):
        view_class = mixin_view(
            "GroupRequiredMixin",
            {"group_required": "test"}
        )
        return view_class

    @pytest.mark.django_db
    def test_success(self, default_view, rf, user):
        request = rf.get("/")
        request.user = user
        response = default_view.as_view()(request)
        assert response.status_code == 200

    @pytest.mark.django_db
    def test_failure(self, default_view, rf, user):
        request = rf.get("/")
        request.user = user
        request.user.groups.clear()
        response = default_view.as_view()(request)
        assert response.status_code == 302

    @pytest.mark.django_db
    def test_no_group_required(self, mixin_view, rf, user):
        _view = mixin_view("GroupRequiredMixin", {"group_required": None})
        request = rf.get("/")

        user.groups.clear()
        request.user = user

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(request)

    def test_group_string(self, default_view):
        assert default_view().get_group_required() == ["test"]


class TestAnonymousRequired:
    @pytest.mark.parametrize("logged_in, status_code", [(False, 200), (True, 302)])
    def test_mixin(self, logged_in, status_code, admin_user, mixin_view, rf):
        _view = mixin_view("AnonymousRequiredMixin")
        request = rf.get("/")
        if logged_in:
            request.user = admin_user
        response = _view.as_view()(request)
        assert response.status_code == status_code



class TestLoginRequired:
    @pytest.mark.parametrize("logged_in, status_code", [(True, 200), (False, 302)])
    def test_mixin(self, logged_in, status_code, admin_user, mixin_view, rf):
        _view = mixin_view("LoginRequiredMixin")
        request = rf.get("/")
        if logged_in:
            request.user = admin_user
        response = _view.as_view()(request)
        assert response.status_code == status_code


class TestRecentLoginRequired:
    @pytest.mark.parametrize(
        "time_gap, status_code",
        [(timedelta(minutes=-10), 200), (timedelta(days=-10), 302)]

    )
    def test_mixin(self, time_gap, status_code, admin_user, rf, mixin_view):
        _view = mixin_view("RecentLoginRequiredMixin")
        _last_login = now() + time_gap
        s = SessionStore()
        s["last_login"] = _last_login.timestamp

        request = rf.get("/")
        request.session = s
        request.user = admin_user
        request.user.last_login = _last_login

        response = _view.as_view()(request)
        assert response.status_code == status_code


@pytest.mark.django_db
class TestPermissionRequired:
    @pytest.mark.django_db
    @pytest.fixture
    def permission(self):
        perm = Permission.objects.get(
            content_type__app_label="project",
            codename="add_article"
        )
        yield perm
        perm.delete()

    @pytest.mark.django_db
    @pytest.fixture
    def user(self, django_user_model):
        u = django_user_model.objects.create_user(
            "test", "Test1234"
        )
        yield u
        u.delete()

    @pytest.fixture
    def default_view(self, mixin_view):
        view_class = mixin_view(
            "PermissionRequiredMixin",
            {"permission_required": {"all": ["project.add_article"]}}
        )
        return view_class

    def test_success(self, default_view, rf, user, permission, django_user_model):
        user.user_permissions.add(permission)
        user = django_user_model.objects.get(pk=user.id)
        request = rf.get("/")
        request.user = user
        response = default_view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, default_view, rf):
        response = default_view.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_no_permission(self, mixin_view, rf, user):
        _view = mixin_view("PermissionRequiredMixin", {})
        request = rf.get("/")
        request.user = user
        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(request)

    def test_optional_permissions(self, mixin_view, rf, user):
        _view = mixin_view(
            "PermissionRequiredMixin",
            {"permission_required": {"any": ["tests.add_article"]}}
        )
        request = rf.get("/")
        request.user = user
        response = _view.as_view()(request)
        assert response.status_code == 200


class TestSSLRequired:
    @pytest.mark.parametrize(
        "is_secure, status_code",
        [(True, 200), (False, 301)]
    )
    def test_mixin(self, is_secure, status_code, mixin_view, rf):
        _view = mixin_view("SSLRequiredMixin")

        request = rf.get("/")
        request.is_secure = lambda: is_secure

        response = _view.as_view()(request)
        assert response.status_code == status_code
