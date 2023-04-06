from datetime import timedelta
import pytest
from django.contrib.auth.models import Group, Permission
from django.contrib.sessions.backends.db import SessionStore
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now


@pytest.mark.mixin("PassesTestMixin")
class TestRequestPassesTest:
    def test_success(self, mixin_view, rf):
        req = rf.get("/")
        _view = mixin_view(request_test="_test")
        _view._test = lambda x: True
        response = _view.as_view()(req)
        assert response.status_code == 200

    def test_failure(self, mixin_view, rf):
        _view = mixin_view(request_test="_test")
        _view._test = lambda x: False

        with pytest.raises(PermissionDenied):
            _view.as_view()(rf.get("/"))

    def test_non_callable(self, mixin_view, rf):
        _view = mixin_view(request_test="not_callable")

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(rf.get("/"))

    def test_missing_method(self, mixin_view, rf):
        with pytest.raises(ImproperlyConfigured):
            mixin_view().as_view()(rf.get("/"))

    def test_none(self, mixin_view, rf):
        _view = mixin_view(request_test=None)

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(rf.get("/"))


@pytest.mark.mixin("SuperuserRequiredMixin")
@pytest.mark.django_db
class TestSuperuserRequired:
    def test_success(self, mixin_view, admin_user, rf):
        request = rf.get("/")
        request.user = admin_user
        response = mixin_view().as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view().as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_superuser(self, mixin_view, django_user_model, rf):
        user = django_user_model.objects.create_user("test", "Test1234")
        request = rf.get("/")
        request.user = user
        response = mixin_view().as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin("StaffUserRequiredMixin")
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
        response = mixin_view().as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        response = mixin_view().as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_non_staff(self, user, mixin_view, rf):
        request = rf.get("/")
        request.user = user
        response = mixin_view().as_view()(request)
        assert response.status_code == 302


@pytest.mark.mixin("GroupRequiredMixin")
@pytest.mark.django_db
class TestGroupRequired:
    def setup_method(self):
        pass

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

    @pytest.mark.django_db
    def test_success(self, mixin_view, rf, user):
        _view = mixin_view(group_required="test")
        request = rf.get("/")
        request.user = user
        response = _view.as_view()(request)
        assert response.status_code == 200

    @pytest.mark.django_db
    def test_failure(self, mixin_view, rf, user):
        _view = mixin_view(group_required="nothing")
        request = rf.get("/")
        request.user = user
        request.user.groups.clear()
        response = _view.as_view()(request)
        assert response.status_code == 302

    @pytest.mark.django_db
    def test_no_group_required(self, mixin_view, rf, user):
        _view = mixin_view(group_required=None)
        request = rf.get("/")

        user.groups.clear()
        request.user = user

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(request)

    def test_group_string(self, mixin_view):
        _view = mixin_view(group_required="test")
        assert _view().get_group_required() == ["test"]


@pytest.mark.mixin("AnonymousRequiredMixin")
class TestAnonymousRequired:
    @pytest.mark.parametrize("logged_in, status_code", [(False, 200), (True, 302)])
    def test_mixin(self, logged_in, status_code, mixin_view, admin_user, rf):
        _view = mixin_view()
        request = rf.get("/")
        if logged_in:
            request.user = admin_user
        response = _view.as_view()(request)
        assert response.status_code == status_code


@pytest.mark.mixin("LoginRequiredMixin")
class TestLoginRequired:
    @pytest.mark.parametrize("logged_in, status_code", [(True, 200), (False, 302)])
    def test_mixin(self, logged_in, status_code, admin_user, mixin_view, rf):
        request = rf.get("/")
        if logged_in:
            request.user = admin_user
        response = mixin_view().as_view()(request)
        assert response.status_code == status_code


@pytest.mark.mixin("RecentLoginRequiredMixin")
class TestRecentLoginRequired:
    @pytest.mark.parametrize(
        "time_gap, status_code",
        [(timedelta(minutes=-10), 200), (timedelta(days=-10), 302)]

    )
    def test_mixin(self, time_gap, status_code, admin_user, rf, mixin_view):
        _last_login = now() + time_gap
        session = SessionStore()
        session["last_login"] = _last_login
        request = rf.get("/")
        request.session = session
        request.user = admin_user
        request.user.last_login = _last_login

        response = mixin_view().as_view()(request)
        assert response.status_code == status_code


@pytest.mark.mixin("PermissionRequiredMixin")
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

    def test_success(self, mixin_view, rf, user, permission, django_user_model):
        user.user_permissions.add(permission)
        user = django_user_model.objects.get(pk=user.id)
        request = rf.get("/")
        request.user = user
        _view = mixin_view(permission_required={"all": ["project.add_article"]})
        response = _view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        _view = mixin_view(permission_required={"all": ["project.add_article"]})
        response = _view.as_view()(rf.get("/"))
        assert response.status_code == 302

    def test_no_permission(self, mixin_view, rf, user):
        request = rf.get("/")
        request.user = user
        with pytest.raises(ImproperlyConfigured):
            mixin_view().as_view()(request)

    def test_optional_permissions(self, mixin_view, rf, user):
        _view = mixin_view(permission_required={"any": ["tests.add_article"]})
        request = rf.get("/")
        request.user = user
        response = _view.as_view()(request)
        assert response.status_code == 200


@pytest.mark.mixin("SSLRequiredMixin")
class TestSSLRequired:
    @pytest.mark.parametrize(
        "is_secure, status_code",
        [(True, 200), (False, 301)]
    )
    def test_mixin(self, is_secure, status_code, mixin_view, rf):
        request = rf.get("/")
        request.is_secure = lambda: is_secure

        response = mixin_view().as_view()(request)
        assert response.status_code == status_code
