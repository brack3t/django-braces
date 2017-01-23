# -*- coding: utf-8 -*-
from __future__ import absolute_import, unicode_literals

import pytest
import datetime

from django import test
from django import VERSION as DJANGO_VERSION
from django.test.utils import override_settings
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.core.urlresolvers import reverse_lazy
from django.http import Http404, HttpResponse

from .compat import force_text
from .factories import GroupFactory, UserFactory
from .helpers import TestViewHelper
from .views import (PermissionRequiredView, MultiplePermissionsRequiredView,
                    SuperuserRequiredView, StaffuserRequiredView,
                    LoginRequiredView, GroupRequiredView, UserPassesTestView,
                    UserPassesTestNotImplementedView, AnonymousRequiredView,
                    SSLRequiredView, RecentLoginRequiredView,
                    UserPassesTestLoginRequiredView)


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
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

    def test_redirects_to_login(self):
        """
        Browser should be redirected to login page if user is not authorized
        to view this page.
        """
        user = self.build_unauthorized_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, '/accounts/login/?next={0}'.format(
            self.view_url))

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
        assert force_text(resp.content) == 'CUSTOM'

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
        resp = self.dispatch_view(req, login_url='/login/')
        self.assertEqual(
            '/login/?next={0}'.format(self.view_url),
            resp['Location'])

        # Test with reverse_lazy
        resp = self.dispatch_view(req, login_url=reverse_lazy('headline'))
        self.assertEqual('/headline/?next={0}'.format(
            self.view_url), resp['Location'])

    def test_custom_redirect_field_name(self):
        """
        Redirect field name should be customizable.
        """
        user = self.build_unauthorized_user()
        req = self.build_request(user=user, path=self.view_url)
        resp = self.dispatch_view(req, redirect_field_name='foo')
        expected_url = '/accounts/login/?foo={0}'.format(self.view_url)
        self.assertEqual(expected_url, resp['Location'])

    @override_settings(LOGIN_URL=None)
    def test_get_login_url_raises_exception(self):
        """
        Test that get_login_url from AccessMixin raises
        ImproperlyConfigured.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(
                self.build_request(path=self.view_url), login_url=None)

    def test_get_redirect_field_name_raises_exception(self):
        """
        Test that get_redirect_field_name from AccessMixin raises
        ImproperlyConfigured.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(
                self.build_request(path=self.view_url),
                redirect_field_name=None)

    @override_settings(LOGIN_URL="/auth/login/")
    def test_overridden_login_url(self):
        """
        Test that login_url is not set in stone on module load but can be
        overridden dynamically.
        """
        user = self.build_unauthorized_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, '/auth/login/?next={0}'.format(
            self.view_url))

    def test_redirect_unauthenticated(self):
        resp = self.dispatch_view(
                self.build_request(path=self.view_url),
                raise_exception=True,
                redirect_unauthenticated_users=True)
        assert resp.status_code == 302
        assert resp['Location'] == '/accounts/login/?next={0}'.format(
            self.view_url)

    def test_redirect_unauthenticated_false(self):
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url),
                raise_exception=True,
                redirect_unauthenticated_users=False)


class TestLoginRequiredMixin(TestViewHelper, test.TestCase):
    """
    Tests for LoginRequiredMixin.
    """
    view_class = LoginRequiredView
    view_url = '/login_required/'

    def test_anonymous(self):
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, '/accounts/login/?next=/login_required/')

    def test_anonymous_raises_exception(self):
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True)

    def test_authenticated(self):
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        assert resp.status_code == 200
        assert force_text(resp.content) == 'OK'

    def test_anonymous_redirects(self):
        resp = self.dispatch_view(
            self.build_request(path=self.view_url), raise_exception=True,
            redirect_unauthenticated_users=True)
        assert resp.status_code == 302
        assert resp['Location'] == '/accounts/login/?next=/login_required/'


class TestChainedLoginRequiredMixin(TestViewHelper, test.TestCase):
    """
    Tests for LoginRequiredMixin combined with another AccessMixin.
    """
    view_class = UserPassesTestLoginRequiredView
    view_url = '/chained_view/'

    def assert_redirect_to_login(self, response):
        """
        Check that the response is a redirect to the login view.
        """
        assert response.status_code == 302
        assert response['Location'] == '/accounts/login/?next=/chained_view/'

    def test_anonymous(self):
        """
        Check that anonymous users redirect to login by default.
        """
        resp = self.dispatch_view(
            self.build_request(path=self.view_url))
        self.assert_redirect_to_login(resp)

    def test_anonymous_raises_exception(self):
        """
        Check that when anonymous users hit a view that has only
        raise_exception set, they get a PermissionDenied.
        """
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True)

    def test_authenticated_raises_exception(self):
        """
        Check that when authenticated users hit a view that has raise_exception
        set, they get a PermissionDenied.
        """
        user = UserFactory()
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url, user=user),
                raise_exception=True)
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url, user=user),
                raise_exception=True, redirect_unauthenticated_users=True)

    def test_anonymous_redirects(self):
        """
        Check that anonymous users are redirected to login when raise_exception
        is overridden by redirect_unauthenticated_users.
        """
        resp = self.dispatch_view(
            self.build_request(path=self.view_url), raise_exception=True,
            redirect_unauthenticated_users=True)
        self.assert_redirect_to_login(resp)


class TestAnonymousRequiredMixin(TestViewHelper, test.TestCase):
    """
    Tests for AnonymousRequiredMixin.
    """
    view_class = AnonymousRequiredView
    view_url = '/unauthenticated_view/'

    def test_anonymous(self):
        """
        As a non-authenticated user, it should be possible to access
        the URL.
        """
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

        # Test with reverse_lazy
        resp = self.dispatch_view(
            self.build_request(),
            login_url=reverse_lazy(self.view_url))
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

    def test_authenticated(self):
        """
        Check that the authenticated user has been successfully directed
        to the approparite view.
        """
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(302, resp.status_code)

        resp = self.client.get(self.view_url, follow=True)
        self.assertRedirects(resp, '/authenticated_view/')

    def test_no_url(self):
        self.view_class.authenticated_redirect_url = None
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        with self.assertRaises(ImproperlyConfigured):
            self.client.get(self.view_url)

    def test_bad_url(self):
        self.view_class.authenticated_redirect_url = '/epicfailurl/'
        user = UserFactory()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(404, resp.status_code)


class TestPermissionRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    """
    Tests for PermissionRequiredMixin.
    """
    view_class = PermissionRequiredView
    view_url = '/permission_required/'

    def build_authorized_user(self):
        return UserFactory(permissions=['auth.add_user'])

    def build_unauthorized_user(self):
        return UserFactory()

    def test_invalid_permission(self):
        """
        ImproperlyConfigured exception should be raised in two situations:
        if permission is None or if permission has invalid name.
        """
        with self.assertRaises(ImproperlyConfigured):
            self.dispatch_view(self.build_request(), permission_required=None)


class TestMultiplePermissionsRequiredMixin(
        _TestAccessBasicsMixin, test.TestCase):
    view_class = MultiplePermissionsRequiredView
    view_url = '/multiple_permissions_required/'

    def build_authorized_user(self):
        return UserFactory(permissions=[
            'tests.add_article', 'tests.change_article', 'auth.change_user'])

    def build_unauthorized_user(self):
        return UserFactory(permissions=['tests.add_article'])

    def test_redirects_to_login(self):
        """
        User should be redirected to login page if he or she does not have
        sufficient permissions.
        """
        url = '/multiple_permissions_required/'
        test_cases = (
            # missing one permission from 'any'
            ['tests.add_article', 'tests.change_article'],
            # missing one permission from 'all'
            ['tests.add_article', 'auth.add_user'],
            # no permissions at all
            [],
        )

        for permissions in test_cases:
            user = UserFactory(permissions=permissions)
            self.client.login(username=user.username, password='asdf1234')
            resp = self.client.get(url)
            self.assertRedirects(resp, '/accounts/login/?next={0}'.format(
                url))

    def test_invalid_permissions(self):
        """
        ImproperlyConfigured exception should be raised if permissions
        attribute is set incorrectly.
        """
        permissions = (
            None,  # permissions must be set
            (),  # and they must be a dict
            {},  # at least one of 'all', 'any' keys must be present
            {'all': None},  # both all and any must be list or a tuple
            {'all': {'a': 1}},
            {'any': None},
            {'any': {'a': 1}},
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
            ['tests.add_article', 'tests.change_article'],
            # missing one permission from 'all'
            ['tests.add_article', 'auth.add_user'],
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
        permissions = {'all': ['auth.add_user', 'tests.add_article']}
        user = UserFactory(permissions=permissions['all'])
        req = self.build_request(user=user)

        resp = self.dispatch_view(req, permissions=permissions)
        self.assertEqual('OK', force_text(resp.content))

        user = UserFactory(permissions=['auth.add_user'])
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(user=user), raise_exception=True,
                permissions=permissions)

    def test_any_permissions_key(self):
        """
        Tests if everything works if only 'any' permissions has been set.
        """
        permissions = {'any': ['auth.add_user', 'tests.add_article']}
        user = UserFactory(permissions=['tests.add_article'])
        req = self.build_request(user=user)

        resp = self.dispatch_view(req, permissions=permissions)
        self.assertEqual('OK', force_text(resp.content))

        user = UserFactory(permissions=[])
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(user=user), raise_exception=True,
                permissions=permissions)


class TestSuperuserRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    view_class = SuperuserRequiredView
    view_url = '/superuser_required/'

    def build_authorized_user(self):
        return UserFactory(is_superuser=True, is_staff=True)

    def build_unauthorized_user(self):
        return UserFactory()


class TestStaffuserRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    view_class = StaffuserRequiredView
    view_url = '/staffuser_required/'

    def build_authorized_user(self):
        return UserFactory(is_staff=True)

    def build_unauthorized_user(self):
        return UserFactory()


class TestGroupRequiredMixin(_TestAccessBasicsMixin, test.TestCase):
    view_class = GroupRequiredView
    view_url = '/group_required/'

    def build_authorized_user(self):
        user = UserFactory()
        group = GroupFactory(name='test_group')
        user.groups.add(group)
        return user

    def build_superuser(self):
        user = UserFactory()
        user.is_superuser = True
        user.save()
        return user

    def build_unauthorized_user(self):
        return UserFactory()

    def test_with_string(self):
        self.assertEqual('test_group', self.view_class.group_required)
        user = self.build_authorized_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

    def test_with_group_list(self):
        group_list = ['test_group', 'editors']
        # the test client will instantiate a new view on request, so we have to
        # modify the class variable (and restore it when the test finished)
        self.view_class.group_required = group_list
        self.assertEqual(group_list, self.view_class.group_required)
        user = self.build_authorized_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))
        self.view_class.group_required = 'test_group'
        self.assertEqual('test_group', self.view_class.group_required)

    def test_superuser_allowed(self):
        user = self.build_superuser()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

    def test_improperly_configured(self):
        view = self.view_class()
        view.group_required = None
        with self.assertRaises(ImproperlyConfigured):
            view.get_group_required()

        view.group_required = {'foo': 'bar'}
        with self.assertRaises(ImproperlyConfigured):
            view.get_group_required()

    def test_with_unicode(self):
        self.view_class.group_required = 'niño'
        self.assertEqual('niño', self.view_class.group_required)

        user = self.build_authorized_user()
        group = user.groups.all()[0]
        group.name = 'niño'
        group.save()
        self.assertEqual('niño', user.groups.all()[0].name)

        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))
        self.view_class.group_required = 'test_group'
        self.assertEqual('test_group', self.view_class.group_required)


class TestUserPassesTestMixin(_TestAccessBasicsMixin, test.TestCase):
    view_class = UserPassesTestView
    view_url = '/user_passes_test/'
    view_not_implemented_class = UserPassesTestNotImplementedView
    view_not_implemented_url = '/user_passes_test_not_implemented/'

    # for testing with passing and not passsing func_test
    def build_authorized_user(self, is_superuser=False):
        return UserFactory(is_superuser=is_superuser, is_staff=True,
                           email="user@mydomain.com")

    def build_unauthorized_user(self):
        return UserFactory()

    def test_with_user_pass(self):
        user = self.build_authorized_user()
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)

        self.assertEqual(200, resp.status_code)
        self.assertEqual('OK', force_text(resp.content))

    def test_with_user_not_pass(self):
        user = self.build_authorized_user(is_superuser=True)
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.view_url)

        self.assertRedirects(resp, '/accounts/login/?next=/user_passes_test/')

    def test_with_user_raise_exception(self):
        with self.assertRaises(PermissionDenied):
            self.dispatch_view(
                self.build_request(path=self.view_url), raise_exception=True)

    def test_not_implemented(self):
        view = self.view_not_implemented_class()
        with self.assertRaises(NotImplementedError):
            view.dispatch(
                self.build_request(path=self.view_not_implemented_url),
                raise_exception=True)


class TestSSLRequiredMixin(test.TestCase):
    view_class = SSLRequiredView
    view_url = '/sslrequired/'

    @pytest.mark.skipif(DJANGO_VERSION[:2] < (1, 9),
                        reason='Django 1.9 and above behave differently')
    def test_ssl_redirection_django_19_up(self):
        self.view_url = 'https://testserver' + self.view_url
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, self.view_url, status_code=301)
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('https', resp.request.get('wsgi.url_scheme'))

    @pytest.mark.skipif(DJANGO_VERSION[:2] < (1, 7),
                        reason='Django 1.6 and below behave differently')
    @pytest.mark.skipif(DJANGO_VERSION[:2] > (1, 8),
                        reason='Django 1.6 and below behave differently')
    def test_ssl_redirection_django_17_up(self):
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertRedirects(resp, self.view_url, status_code=301)
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('https', resp.request.get('wsgi.url_scheme'))

    @pytest.mark.skipif(DJANGO_VERSION[:2] > (1, 6),
                        reason='Django 1.7 and above behave differently')
    def test_ssl_redirection_django_16_down(self):
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertEqual(301, resp.status_code)
        resp = self.client.get(self.view_url, follow=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('https', resp.request.get('wsgi.url_scheme'))

    def test_raises_exception(self):
        self.view_class.raise_exception = True
        resp = self.client.get(self.view_url)
        self.assertEqual(404, resp.status_code)

    @override_settings(DEBUG=True)
    def test_debug_bypasses_redirect(self):
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url)
        self.assertEqual(200, resp.status_code)

    @pytest.mark.skipif(
        DJANGO_VERSION[:2] < (1, 7),
        reason='Djanog 1.6 and below does not have the secure=True option')
    def test_https_does_not_redirect_django_17_up(self):
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url, secure=True)
        self.assertEqual(200, resp.status_code)
        self.assertEqual('https', resp.request.get('wsgi.url_scheme'))

    @pytest.mark.skipif(
        DJANGO_VERSION[:2] > (1, 6),
        reason='Django 1.7 and above have secure=True option, below does not')
    def test_https_does_not_redirect_django_16_down(self):
        self.view_class.raise_exception = False
        resp = self.client.get(self.view_url, **{'wsgi.url_scheme': 'https'})
        self.assertEqual(200, resp.status_code)
        self.assertEqual('https', resp.request.get('wsgi.url_scheme'))


class TestRecentLoginRequiredMixin(test.TestCase):
    """
    Tests for RecentLoginRequiredMixin.
    """
    view_class = RecentLoginRequiredView
    recent_view_url = '/recent_login/'
    outdated_view_url = '/outdated_login/'

    def test_recent_login(self):
        self.view_class.max_last_login_delta = 1800
        last_login = datetime.datetime.now()
        user = UserFactory(last_login=last_login)
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.recent_view_url)
        assert resp.status_code == 200
        assert force_text(resp.content) == 'OK'

    def test_outdated_login(self):
        self.view_class.max_last_login_delta = 0
        last_login = datetime.datetime.now() - datetime.timedelta(hours=2)
        user = UserFactory(last_login=last_login)
        self.client.login(username=user.username, password='asdf1234')
        resp = self.client.get(self.outdated_view_url)
        assert resp.status_code == 302
        
    def test_not_logged_in(self):
        last_login = datetime.datetime.now()
        user = UserFactory(last_login=last_login)
        resp = self.client.get(self.recent_view_url)
        assert resp.status_code != 200
