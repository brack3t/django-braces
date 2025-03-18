import datetime

import pytest

from django import test
from django.contrib.auth.models import Permission
from django.test.utils import override_settings
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.http import Http404, HttpResponse
from django.utils.encoding import force_str
from django.utils.timezone import make_aware, get_current_timezone

from django.urls import reverse_lazy

from .factories import (
    GroupFactory,
    UserFactory,
    UserObjectPermissionsFactory,
    ArticleFactory,
)
from .helpers import TestViewHelper
from .views import (
    PermissionRequiredView,
    MultiplePermissionsRequiredView,
    SuperuserRequiredView,
    StaffuserRequiredView,
    LoginRequiredView,
    GroupRequiredView,
    UserPassesTestView,
    UserPassesTestNotImplementedView,
    AnonymousRequiredView,
    SSLRequiredView,
    RecentLoginRequiredView,
    UserPassesTestLoginRequiredView,
)


@pytest.mark.django_db
class _TestAccessBasicsMixin(TestViewHelper):
    """
    A set of basic tests for access mixins.
    """

    view_url = None

    def build_authorized_user(self):
        """
        Returns user authorized to access view.
        """
        raise NotImplementedError

    def build_unauthorized_user(self):
        """
        Returns user not authorized to access view.
        """
        raise NotImplementedError

    def test_success(self):
        """
        If user is authorized then view should return normal response.
        """
        user = self.build_authorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

    def test_redirects_to_login(self):
        """
        Browser should be redirected to login page if user is not authorized
        to view this page.
        """
        user = self.build_unauthorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, "/accounts/login/?next={0}".format(self.view_url))

    def test_raise_permission_denied(self):
        """
        PermissionDenied should be raised if user is not authorized and
        raise_exception attribute is set to True.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        with self.assertRaises(PermissionDenied):
            self.dispatch_view(req, raise_exception=True)

    def test_raise_custom_exception(self):
        """
        Http404 should be raised if user is not authorized and
        raise_exception attribute is set to Http404.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        with self.assertRaises(Http404):
            self.dispatch_view(req, raise_exception=Http404)

    def test_raise_func_pass(self):
        """
        An exception should be raised if user is not authorized and
        raise_exception attribute is set to a function that returns nothing.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        def func(request):
            pass

        with self.assertRaises(PermissionDenied):
            self.dispatch_view(req, raise_exception=func)

    def test_raise_func_response(self):
        """
        A custom response should be returned if user is not authorized and
        raise_exception attribute is set to a function that returns a response.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        def func(request):
            return HttpResponse("CUSTOM")

        resp = self.dispatch_view(req, raise_exception=func)
        assert resp.status_code == 200
        assert force_str(resp.content) == "CUSTOM"

    def test_raise_func_false(self):
        """
        PermissionDenied should be raised, if a custom raise_exception
        function does not return HttpResponse or StreamingHttpResponse.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        def func(request):
            return False

        with self.assertRaises(PermissionDenied):
            self.dispatch_view(req, raise_exception=func)

    def test_raise_func_raises(self):
        """
        A custom exception should be raised if user is not authorized and
        raise_exception attribute is set to a callable that raises an
        exception.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)

        def func(request):
            raise Http404

        with self.assertRaises(Http404):
            self.dispatch_view(req, raise_exception=func)

    def test_custom_login_url(self):
        """
        Login url should be customizable.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)
        resp = self.dispatch_view(req, login_url="/login/")
        self.assertEqual("/login/?next={0}".format(self.view_url), resp["Location"])

        # Test with reverse_lazy
        resp = self.dispatch_view(req, login_url=reverse_lazy("headline"))
        self.assertEqual("/headline/?next={0}".format(self.view_url), resp["Location"])

    def test_custom_redirect_field_name(self):
        """
        Redirect field name should be customizable.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)
        resp = self.dispatch_view(req, redirect_field_name="foo")
        expected_url = "/accounts/login/?foo={0}".format(self.view_url)
        self.assertEqual(expected_url, resp["Location"])

    @override_settings(LOGIN_URL=None)
    def test_get_login_url_raises_exception(self):
        """
        Test that get_login_url from AccessMixin raises
        ImproperlyConfigured.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(path=self.view_url), login_url=None)

    def test_get_redirect_field_name_raises_exception(self):
        """
        Test that get_redirect_field_name from AccessMixin raises
        ImproperlyConfigured.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(
                self.build_request(path=self.view_url),
                redirect_field_name=None,
            )

    @override_settings(LOGIN_URL="/auth/login/")
    def test_overridden_login_url(self):
        """
        Test that login_url is not set in stone on module load but can be
        overridden dynamically.
        """
        user = self.build_unauthorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, "/auth/login/?next={0}".format(self.view_url))

    def test_redirect_unauthenticated(self):
        resp = self.dispatch_view(
            self.build_request(path=self.view_url),
            raise_exception=True,
            redirect_unauthenticated_users=True,
        )
        assert resp.status_code == 302
        assert resp["Location"] == "/accounts/login/?next={0}".format(self.view_url)

    def test_redirect_unauthenticated_false(self):
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url),
                raise_exception=True,
                redirect_unauthenticated_users=False,
            )


@pytest.mark.django_db
class TestLoginRequiredMixin(TestViewHelper, test.TestCase):
    """Scenarios around requiring an authenticated session"""

    view_class = LoginRequiredView
    view_url = "/login_required/"

    def test_anonymous(self):
        """Anonymous users should be redirected"""
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, "/accounts/login/?next=/login_required/")

    def test_anonymous_raises_exception(self):
        """Anonymous users should raise an exception"""
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True
            )

    def test_authenticated(self):
        """Authenticated users should get 'OK'"""
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        assert resp.status_code == 200
        assert force_str(resp.content) == "OK"

    def test_anonymous_redirects(self):
        """Anonymous users are redirected with a 302"""
        resp = self.dispatch_view(
            self.build_request(path=self.view_url),
            raise_exception=True,
            redirect_unauthenticated_users=True,
        )
        assert resp.status_code == 302
        assert resp["Location"] == "/accounts/login/?next=/login_required/"


@pytest.mark.django_db
class TestChainedLoginRequiredMixin(TestViewHelper, test.TestCase):
    """
    Tests for LoginRequiredMixin combined with another AccessMixin.
    """

    view_class = UserPassesTestLoginRequiredView
    view_url = "/chained_view/"

    def assert_redirect_to_login(self, response):
        """
        Check that the response is a redirect to the login view.
        """
        assert response.status_code == 302
        assert response["Location"] == "/accounts/login/?next=/chained_view/"

    def test_anonymous(self):
        """
        Check that anonymous users redirect to login by default.
        """
        resp = self.dispatch_view(self.build_request(path=self.view_url))
        self.assert_redirect_to_login(resp)

    def test_anonymous_raises_exception(self):
        """
        Check that when anonymous users hit a view that has only
        raise_exception set, they get a PermissionDenied.
        """
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True
            )

    def test_authenticated_raises_exception(self):
        """
        Check that when authenticated users hit a view that has raise_exception
        set, they get a PermissionDenied.
        """
        user = UserFactory()
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url, user=user),
                raise_exception=True,
            )
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url, user=user),
                raise_exception=True,
                redirect_unauthenticated_users=True,
            )

    def test_anonymous_redirects(self):
        """
        Check that anonymous users are redirected to login when raise_exception
        is overridden by redirect_unauthenticated_users.
        """
        resp = self.dispatch_view(
            self.build_request(path=self.view_url),
            raise_exception=True,
            redirect_unauthenticated_users=True,
        )
        self.assert_redirect_to_login(resp)


@pytest.mark.django_db
class TestAnonymousRequiredMixin(TestViewHelper, test.TestCase):
    """
    Tests for AnonymousRequiredMixin.
    """

    view_class = AnonymousRequiredView
    view_url = "/unauthenticated_view/"

    def test_anonymous(self):
        """
        As a non-authenticated user, it should be possible to access
        the URL.
        """
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

        # Test with reverse_lazy
        resp = self.dispatch_view(
            self.build_request(), login_url=reverse_lazy(self.view_url)
        )
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

    def test_authenticated(self):
        """
        Check that the authenticated user has been successfully directed
        to the appropriate view.
        """
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(302, resp.status_code)

        resp = self.client.get(self.view_url, follow=True)
        self.assertRedirects(resp, "/authenticated_view/")

    def test_no_url(self):
        """View should raise an exception if no URL is provided"""
        self.view_class.authenticated_redirect_url = None
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_bad_url(self):
        """Redirection can be misconfigured"""
        self.view_class.authenticated_redirect_url = "/epicfailurl/"
        user = UserFactory()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(404, resp.status_code)


@pytest.mark.django_db
class TestPermissionRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios around requiring a permission"""

    view_class = PermissionRequiredView
    view_url = "/permission_required/"

    def build_authorized_user(self):
        """Create a user with permissions"""
        return UserFactory(permissions=["auth.add_user"])

    def build_unauthorized_user(self):
        """Create a user without permissions"""
        return UserFactory()

    def test_invalid_permission(self):
        """
        ImproperlyConfigured exception should be raised in two situations:
        if permission is None or if permission has invalid name.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), permission_required=None)

    def test_object_level_permissions(self):
        """
        Tests that object level permissions perform as expected, where object level permissions and
        global level permissions
        """
        # Arrange
        article = ArticleFactory()
        self.view_class = PermissionRequiredView
        self.view_url = f"/object_level_permission_required/?pk={article.pk}"
        tests_add_article = Permission.objects.get(codename="add_article")
        permissions = "tests.add_article"
        valid_user = UserFactory(permissions=[permissions])
        invalid_user_1 = UserFactory(permissions=["auth.add_user"])
        invalid_user_2 = UserFactory(permissions=[permissions])
        UserObjectPermissionsFactory(
            user=valid_user, permission=tests_add_article, article_object=article
        )
        # Act
        valid_req = self.build_request(path=self.view_url, user=valid_user)
        valid_resp = self.dispatch_view(
            valid_req,
            permission_required=permissions,
            object_level_permissions=True,
            raise_exception=True,
        )
        invalid_req_1 = self.build_request(path=self.view_url, user=invalid_user_1)
        invalid_req_2 = self.build_request(path=self.view_url, user=invalid_user_2)
        # Assert
        self.assertEqual(valid_resp.status_code, 200)
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                invalid_req_1,
                permission_required=permissions,
                object_level_permissions=True,
                raise_exception=True,
            )
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                invalid_req_2,
                permission_required=permissions,
                object_level_permissions=True,
                raise_exception=True,
            )


@pytest.mark.django_db
class TestMultiplePermissionsRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios around requiring multiple permissions"""

    view_class = MultiplePermissionsRequiredView
    view_url = "/multiple_permissions_required/"

    def build_authorized_user(self):
        """Get a user with permissions"""
        return UserFactory(
            permissions=[
                "tests.add_article",
                "tests.change_article",
                "auth.change_user",
            ]
        )

    def build_unauthorized_user(self):
        """Get a user without the important permissions"""
        return UserFactory(permissions=["tests.add_article"])

    def test_redirects_to_login(self):
        """
        User should be redirected to login page if he or she does not have
        sufficient permissions.
        """
        url = "/multiple_permissions_required/"
        test_cases = (
            # missing one permission from 'any'
            ["tests.add_article", "tests.change_article"],
            # missing one permission from 'all'
            ["tests.add_article", "auth.add_user"],
            # no permissions at all
            [],
        )

        for permissions in test_cases:
            user = UserFactory(permissions=permissions)
            self.client.login(username=user.username, password="asdf1234")
            resp = self.client.get(url)
            self.assertRedirects(resp, "/accounts/login/?next={0}".format(url))

    def test_invalid_permissions(self):
        """
        ImproperlyConfigured exception should be raised if permissions
        attribute is set incorrectly.
        """
        permissions = (
            None,  # permissions must be set
            (),  # and they must be a dict
            {},  # at least one of 'all', 'any' keys must be present
            {"all": None},  # both all and any must be list or a tuple
            {"all": {"a": 1}},
            {"any": None},
            {"any": {"a": 1}},
        )

        for attr in permissions:
            with self.assertRaises(ImproperlyConfigured):
                self.dispatch_view(self.build_request(), permissions=attr)

    def test_raise_permission_denied(self):
        """
        PermissionDenied should be raised if user does not have sufficient
        permissions and raise_exception is set to True.
        """
        test_cases = (
            # missing one permission from 'any'
            ["tests.add_article", "tests.change_article"],
            # missing one permission from 'all'
            ["tests.add_article", "auth.add_user"],
            # no permissions at all
            [],
        )

        for permissions in test_cases:
            user = UserFactory(permissions=permissions)
            req = self.build_request(user=user)
            with self.assertRaises(PermissionDenied):
                self.dispatch_view(req, raise_exception=True)

    def test_all_permissions_key(self):
        """
        Tests if everything works if only 'all' permissions has been set.
        """
        permissions = {"all": ["auth.add_user", "tests.add_article"]}
        user = UserFactory(permissions=permissions["all"])
        req = self.build_request(user=user)

        resp = self.dispatch_view(req, permissions=permissions)
        self.assertEqual("OK", force_str(resp.content))

        user = UserFactory(permissions=["auth.add_user"])
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(user=user),
                raise_exception=True,
                permissions=permissions,
            )

    def test_any_permissions_key(self):
        """
        Tests if everything works if only 'any' permissions has been set.
        """
        permissions = {"any": ["auth.add_user", "tests.add_article"]}
        user = UserFactory(permissions=["tests.add_article"])
        req = self.build_request(user=user)

        resp = self.dispatch_view(req, permissions=permissions)
        self.assertEqual("OK", force_str(resp.content))

        user = UserFactory(permissions=[])
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(user=user),
                raise_exception=True,
                permissions=permissions,
            )

    def test_all_object_level_permissions_key(self):
        """
        Tests that when a user has all the correct object level permissions, response is OK,
        else forbidden.
        """
        # Arrange
        article = ArticleFactory()
        self.view_class = MultiplePermissionsRequiredView
        self.view_url = f"/multiple_object_level_permissions_required/?pk={article.pk}"
        auth_add_user = Permission.objects.get(codename="add_user")
        tests_add_article = Permission.objects.get(codename="add_article")
        permissions = {"all": ["auth.add_user", "tests.add_article"]}
        valid_user = UserFactory(permissions=permissions["all"])
        invalid_user = UserFactory(permissions=["auth.add_user"])
        UserObjectPermissionsFactory(
            user=valid_user, permission=auth_add_user, article_object=article
        )
        UserObjectPermissionsFactory(
            user=valid_user, permission=tests_add_article, article_object=article
        )
        # Act
        valid_req = self.build_request(path=self.view_url, user=valid_user)
        valid_resp = self.dispatch_view(
            valid_req, permissions=permissions, object_level_permissions=True
        )
        invalid_req = self.build_request(path=self.view_url, user=invalid_user)
        # Arrange
        self.assertEqual(valid_resp.status_code, 200)
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                invalid_req,
                permissions=permissions,
                object_level_permissions=True,
                raise_exception=True,
            )

    def test_any_object_level_permissions_key(self):
        """
        Tests that when a user has any the correct object level permissions, response is OK,
        else forbidden.
        """
        # Arrange
        article = ArticleFactory()
        self.view_url = f"/multiple_object_level_permissions_required/?pk={article.pk}"
        self.view_class = MultiplePermissionsRequiredView
        auth_add_user = Permission.objects.get(codename="add_user")
        tests_add_article = Permission.objects.get(codename="add_article")
        permissions = {"any": ["auth.add_user", "tests.add_article"]}
        user = UserFactory(permissions=[permissions["any"][0]])
        user_1 = UserFactory()
        user_2 = UserFactory(permissions=permissions["any"])
        UserObjectPermissionsFactory(
            user=user, permission=auth_add_user, article_object=article
        )
        UserObjectPermissionsFactory(
            user=user, permission=tests_add_article, article_object=article
        )
        # Act
        valid_req = self.build_request(path=self.view_url, user=user)
        valid_resp = self.dispatch_view(
            valid_req,
            permissions=permissions,
            object_level_permissions=True,
            raise_exception=True,
        )
        invalid_req_1 = self.build_request(path=self.view_url, user=user_1)
        invalid_req_2 = self.build_request(path=self.view_url, user=user_2)
        # Assert
        self.assertEqual(valid_resp.status_code, 200)
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                invalid_req_1,
                permissions=permissions,
                object_level_permissions=True,
                raise_exception=True,
            )
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                invalid_req_2,
                permissions=permissions,
                object_level_permissions=True,
                raise_exception=True,
            )


@pytest.mark.django_db
class TestSuperuserRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios requiring a superuser"""

    view_class = SuperuserRequiredView
    view_url = "/superuser_required/"

    def build_authorized_user(self):
        """Make a superuser"""
        return UserFactory(is_superuser=True, is_staff=True)

    def build_unauthorized_user(self):
        """Make a non-superuser"""
        return UserFactory()


@pytest.mark.django_db
class TestStaffuserRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios requiring a staff user"""

    view_class = StaffuserRequiredView
    view_url = "/staffuser_required/"

    def build_authorized_user(self):
        """Hire a user"""
        return UserFactory(is_staff=True)

    def build_unauthorized_user(self):
        """Get a customer"""
        return UserFactory()


@pytest.mark.django_db
class TestGroupRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios requiring membership in a certain group"""

    view_class = GroupRequiredView
    view_url = "/group_required/"

    def build_authorized_user(self):
        """Get a user with the right group"""
        user = UserFactory()
        group = GroupFactory(name="test_group")
        user.groups.add(group)
        return user

    def build_superuser(self):
        """Get a superuser"""
        user = UserFactory()
        user.is_superuser = True
        user.save()
        return user

    def build_unauthorized_user(self):
        """Just a normal users, not super and no groups"""
        return UserFactory()

    def test_with_string(self):
        """A group name as a string should restrict access"""
        self.assertEqual("test_group", self.view_class.group_required)
        user = self.build_authorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

    def test_with_group_list(self):
        """A list of group names should restrict access"""
        group_list = ["test_group", "editors"]
        # the test client will instantiate a new view on request, so we have to
        # modify the class variable (and restore it when the test finished)
        self.view_class.group_required = group_list
        self.assertEqual(group_list, self.view_class.group_required)
        user = self.build_authorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))
        self.view_class.group_required = "test_group"
        self.assertEqual("test_group", self.view_class.group_required)

    def test_superuser_allowed(self):
        """Superusers should always be allowed, regardless of group rules"""
        user = self.build_superuser()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

    def test_improperly_configured(self):
        """No group(s) specified should raise ImproperlyConfigured"""
        view = self.view_class()
        view.group_required = None
        with self.assertRaises(ImproperlyConfigured):
            view.get_group_required()

        view.group_required = {"foo": "bar"}
        with self.assertRaises(ImproperlyConfigured):
            view.get_group_required()

    def test_with_unicode(self):
        """Unicode in group names should restrict access"""
        self.view_class.group_required = "niño"
        self.assertEqual("niño", self.view_class.group_required)

        user = self.build_authorized_user()
        group = user.groups.all()[0]
        group.name = "niño"
        group.save()
        self.assertEqual("niño", user.groups.all()[0].name)

        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))
        self.view_class.group_required = "test_group"
        self.assertEqual("test_group", self.view_class.group_required)


@pytest.mark.django_db
class TestUserPassesTestMixin(_TestAccessBasicsMixin, test.TestCase):
    """Scenarios requiring a user to pass a test"""

    view_class = UserPassesTestView
    view_url = "/user_passes_test/"
    view_not_implemented_class = UserPassesTestNotImplementedView
    view_not_implemented_url = "/user_passes_test_not_implemented/"

    # for testing with passing and not passing func_test
    def build_authorized_user(self, is_superuser=False):
        """Get a test-passing user"""
        return UserFactory(
            is_superuser=is_superuser, is_staff=True, email="user@mydomain.com"
        )

    def build_unauthorized_user(self):
        """Get a blank user"""
        return UserFactory()

    def test_with_user_pass(self):
        """Valid username and password should pass the test"""
        user = self.build_authorized_user()
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)

        self.assertEqual(200, resp.status_code)
        self.assertEqual("OK", force_str(resp.content))

    def test_with_user_not_pass(self):
        """A failing user should be redirected"""
        user = self.build_authorized_user(is_superuser=True)
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.view_url)

        self.assertRedirects(resp, "/accounts/login/?next=/user_passes_test/")

    def test_with_user_raise_exception(self):
        """PermissionDenied should be raised"""
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True
            )

    def test_not_implemented(self):
        """NotImplemented should be raised"""
        view = self.view_not_implemented_class()
        with self.assertRaises(NotImplementedError):
            view.dispatch(
                self.build_request(path=self.view_not_implemented_url),
                raise_exception=True,
            )


@pytest.mark.django_db
class TestSSLRequiredMixin(test.TestCase):
    """Scenarios around requiring SSL"""

    view_class = SSLRequiredView
    view_url = "/sslrequired/"

    def test_ssl_redirection(self):
        """Should redirect if not SSL"""
        self.view_url = f"https://testserver{self.view_url}"
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, self.view_url, status_code=301)
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("https", resp.request.get("wsgi.url_scheme"))

    def test_raises_exception(self):
        """Should return 404"""
        self.view_class.raise_exception = True
        resp = self.client.get(self.view_url)
        self.assertEqual(404, resp.status_code)

    @override_settings(DEBUG=True)
    def test_debug_bypasses_redirect(self):
        """Debug mode should not require SSL"""
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)

    def test_https_does_not_redirect(self):
        """SSL requests should not redirect"""
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url, secure=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual("https", resp.request.get("wsgi.url_scheme"))


@pytest.mark.django_db
class TestRecentLoginRequiredMixin(test.TestCase):
    """Scenarios requiring a recent login"""

    view_class = RecentLoginRequiredView
    recent_view_url = "/recent_login/"
    outdated_view_url = "/outdated_login/"

    def test_recent_login(self):
        """A recent login should get a 200"""
        self.view_class.max_last_login_delta = 1800
        last_login = datetime.datetime.now()
        last_login = make_aware(last_login, get_current_timezone())
        user = UserFactory(last_login=last_login)
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.recent_view_url)
        assert resp.status_code == 200
        assert force_str(resp.content) == "OK"

    def test_outdated_login(self):
        """An outdated login should get a 302"""
        self.view_class.max_last_login_delta = 0
        last_login = datetime.datetime.now() - datetime.timedelta(hours=2)
        last_login = make_aware(last_login, get_current_timezone())
        user = UserFactory(last_login=last_login)
        self.client.login(username=user.username, password="asdf1234")
        resp = self.client.get(self.outdated_view_url)
        assert resp.status_code in [
            302,
            405,
        ]  # 302 is for Django < 5, while 405 is for Django >= 5

    def test_not_logged_in(self):
        """Anonymous requests should be handled appropriately"""
        last_login = datetime.datetime.now()
        last_login = make_aware(last_login, get_current_timezone())
        resp = self.client.get(self.recent_view_url)
        assert resp.status_code != 200
