"""Tests for the access-related mixins."""
from datetime import timedelta

import pytest
from django.contrib.auth.models import Group, Permission
from django.contrib.sessions.backends.db import SessionStore
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.utils.timezone import now


@pytest.mark.mixin("PassesTestMixin")
class TestPassesTestMixin:
    """Test for the `PassesTestMixin`."""

    def test_success(self, mixin_view, rf):
        """A `True` test has a 200 status code."""
        _view = mixin_view(dispatch_test="success")
        _view.success = lambda x: True
        req = rf.get("/")
        response = _view.as_view()(req)
        assert response.status_code == 200

    def test_failure(self, mixin_view, rf):
        """A `False` test raises an exception."""
        _view = mixin_view(dispatch_test="failure")
        _view.failure = lambda x: False

        with pytest.raises(PermissionDenied):
            _view.as_view()(rf.get("/"))

    def test_non_callable(self, mixin_view, rf):
        """A non-callable test raises an exception."""
        _view = mixin_view(dispatch_test="not_callable")
        _view.not_callable = "test"

        with pytest.raises(ImproperlyConfigured) as exc:
            _view.as_view()(rf.get("/"))
        assert "must be a callable." in str(exc)

    def test_missing_method(self, mixin_view, rf):
        """A view without a `dispatch_test` raises an exception."""
        with pytest.raises(ImproperlyConfigured):
            mixin_view().as_view()(rf.get("/"))

    def test_none(self, mixin_view, rf):
        """A view with `dispatch_test=None` raises an exception."""
        _view = mixin_view(dispatch_test=None)

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(rf.get("/"))

    def test_attribute_error(self, mixin_view, rf):
        """A view with `dispatch_test=None` raises an exception."""
        _view = mixin_view(dispatch_test="not_a_method")

        with pytest.raises(ImproperlyConfigured) as exc:
            _view.as_view()(rf.get("/"))
        assert "is missing the `not_a_method` method." in str(exc)


@pytest.mark.mixin("SuperuserRequiredMixin")
@pytest.mark.django_db()
class TestSuperuserRequired:
    """Tests for the `SuperuserRequiredMixin`."""

    def test_success(self, mixin_view, admin_user, rf):
        """A superuser should have access."""
        request = rf.get("/")
        request.user = admin_user
        response = mixin_view().as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        """There is no such thing as an anonymous superuser."""
        with pytest.raises(PermissionDenied):
            mixin_view().as_view()(rf.get("/"))

    def test_non_superuser(self, mixin_view, django_user_model, rf):
        """Users without superuser status should be denied."""
        user = django_user_model.objects.create_user("test", "Test1234")
        request = rf.get("/")
        request.user = user
        with pytest.raises(PermissionDenied):
            mixin_view().as_view()(request)


@pytest.mark.mixin("StaffUserRequiredMixin")
@pytest.mark.django_db()
class TestStaffUserRequired:
    """Tests related to the `StaffUserRequiredMixin`."""

    def test_success(self, user, mixin_view, rf):
        """Users marked as staff should have access."""
        request = rf.get("/")
        user.is_staff = True
        request.user = user
        response = mixin_view().as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        """Anonymous users aren't staff and should be denied."""
        with pytest.raises(PermissionDenied):
            mixin_view().as_view()(rf.get("/"))

    def test_non_staff(self, user, mixin_view, rf):
        """Users without staff status should be denied."""
        request = rf.get("/")
        request.user = user
        with pytest.raises(PermissionDenied):
            mixin_view().as_view()(request)


@pytest.mark.mixin("GroupRequiredMixin")
@pytest.mark.django_db()
class TestGroupRequired:
    """Tests relating to the `GroupRequiredMixin`."""

    @pytest.fixture()
    def group(self):
        """Generate a group fixture."""
        g = Group.objects.create(name="test")
        yield g
        g.delete()

    @pytest.fixture()
    def user(self, group, user):
        """Generate a user with a group fixture."""
        user.groups.add(group)
        user.refresh_from_db()
        assert user.groups.count() == 1
        return user

    def test_success(self, mixin_view, rf, user):
        """Members of the group should be allowed."""
        _view = mixin_view(group_required="test")
        request = rf.get("/")
        request.user = user
        response = _view.as_view()(request)
        assert response.status_code == 200

    def test_failure(self, mixin_view, rf, user):
        """Users who are not members are denied."""
        _view = mixin_view(group_required="nothing")
        request = rf.get("/")
        request.user = user
        request.user.groups.clear()
        with pytest.raises(PermissionDenied):
            _view.as_view()(request)

    def test_no_group_required(self, mixin_view, rf, user):
        """If `group_required` is missing, raise an exception."""
        _view = mixin_view(group_required=None)
        request = rf.get("/")

        user.groups.clear()
        request.user = user

        with pytest.raises(ImproperlyConfigured):
            _view.as_view()(request)

    def test_group_string(self, mixin_view):
        """A string should be converted to a list."""
        _view = mixin_view(group_required="test")
        assert _view().get_group_required() == ["test"]


@pytest.mark.mixin("AnonymousRequiredMixin")
class TestAnonymousRequired:
    """Test mixins requiring anonymous users."""

    @pytest.mark.parametrize(("logged_in", "status_code"), [(False, 200), (True, 302)])
    def test_mixin(self, logged_in: bool, status_code: int, mixin_view, admin_user, rf):
        """AnonymousRequiredMixin should error for authenticated users."""
        _view = mixin_view().as_view()
        user = admin_user if logged_in else None
        request = rf.get("/")
        request.user = user
        response = None

        try:
            response = _view(request)
        except PermissionDenied:
            if status_code == 200:
                raise
        else:
            assert response.status_code == status_code


@pytest.mark.mixin("LoginRequiredMixin")
class TestLoginRequired:
    """Tests related to the `LoginRequiredMixin`."""

    @pytest.mark.parametrize(("logged_in", "status_code"), [(True, 200), (False, 302)])
    def test_mixin(self, logged_in, status_code, admin_user, mixin_view, rf):
        """Unauthenticated users should be denied."""
        request = rf.get("/")
        if logged_in:
            request.user = admin_user
        try:
            response = mixin_view().as_view()(request)
        except PermissionDenied:
            if status_code == 200:
                raise
        else:
            assert response.status_code == status_code


@pytest.mark.mixin("RecentLoginRequiredMixin")
class TestRecentLoginRequired:
    """Tests related to the `RecentLoginRequiredMixin`."""

    @pytest.mark.parametrize(
        ("time_gap", "status_code"),
        [(timedelta(minutes=-10), 200), (timedelta(days=-10), 302)],
    )
    def test_mixin(self, time_gap, status_code, admin_user, rf, mixin_view):
        """Users with a recent login should be allowed."""
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
@pytest.mark.django_db()
class TestPermissionRequired:
    """Tests related to the `PermissionRequiredMixin`."""

    @pytest.fixture()
    def permission(self):
        """Generate a permission fixture."""
        perm = Permission.objects.get(
            content_type__app_label="project",
            codename="add_article",
        )
        yield perm
        perm.delete()

    def test_success(self, mixin_view, rf, user, permission, django_user_model):
        """Users with appropriate permissions should be allowed."""
        user.user_permissions.add(permission)
        user = django_user_model.objects.get(pk=user.id)
        request = rf.get("/")
        request.user = user
        _view = mixin_view(permission_required={"all": ["project.add_article"]})
        response = _view.as_view()(request)
        assert response.status_code == 200

    def test_anonymous(self, mixin_view, rf):
        """Anonymous users should be denied."""
        _view = mixin_view(permission_required={"all": ["project.add_article"]})
        with pytest.raises(PermissionDenied):
            _view.as_view()(rf.get("/"))

    def test_no_permission(self, mixin_view, rf, user):
        """Users with no permissions should be denied."""
        request = rf.get("/")
        request.user = user
        with pytest.raises(ImproperlyConfigured):
            mixin_view().as_view()(request)

    def test_optional_permissions(self, mixin_view, rf, user):
        """Optional permissions should be allowed."""
        _view = mixin_view(permission_required={"any": ["tests.add_article"]})
        request = rf.get("/")
        request.user = user
        response = _view.as_view()(request)
        assert response.status_code == 200


@pytest.mark.mixin("SSLRequiredMixin")
class TestSSLRequired:
    """Tests related to the `SSLRequiredMixin`."""

    @pytest.mark.parametrize(("is_secure", "status_code"), [(True, 200), (False, 301)])
    def test_mixin(self, is_secure, status_code, mixin_view, rf):
        """The view should be allowed if the request is secure."""
        request = rf.get("/")
        request.is_secure = lambda: is_secure

        response = mixin_view().as_view()(request)
        assert response.status_code == status_code
