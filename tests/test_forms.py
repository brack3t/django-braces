import pytest
from django import forms
from django.views.generic import FormView

from braces import mixins



class TestUserFormMixin:
    def test_invalid_class(self):
        class InvalidForm(mixins.UserFormMixin):
            pass

        with pytest.raises(TypeError):
            InvalidForm()

    def test_form_has_user(self, admin_user):
        form = TestFormWithUserMixin._Form(user=admin_user)
        assert form.user == admin_user


class TestFormWithUserMixin:
    class _Form(mixins.UserFormMixin, forms.Form):
        pass

    class _View(mixins.FormWithUserMixin, FormView):
        def get_form_class(self):
            return TestFormWithUserMixin._Form

    def test_user_to_form_kwargs(self, admin_user, rf):
        request = rf.get("/")
        request.user = admin_user
        view = self._View()
        view.setup(request)
        assert view.get_form_kwargs()["user"] == admin_user

    def test_user_to_form_wrapped_class(self, admin_user, rf):
        class UserlessForm(forms.Form):
            pass

        request = rf.get("/")
        request.user = admin_user
        view = self._View()
        view.form_class = UserlessForm
        view.setup(request)
        assert issubclass(view.get_form_class(), mixins.UserFormMixin)


class TestCSRFExempt:
    class _View(mixins.CSRFExemptMixin, FormView):
        success_url = "/"
        def get_form_class(self):
            return TestCSRFExempt._Form

    class _Form(forms.Form):
        pass

    def test_csrf_exempt(self, rf):
        request = rf.post("/")
        view = self._View()
        view.setup(request)
        assert view.dispatch(request).status_code == 302

    def test_csrf_exempt_with_form(self, rf):
        class MyForm(forms.Form):
            pass

        request = rf.post("/")
        view = self._View()
        view.setup(request)
        assert view.dispatch(request).status_code == 302


class TestSuccessURLRedirect:
    class _View(mixins.SuccessURLRedirectMixin, FormView):
        success_url = "/"

    def test_success_url(self, rf):
        request = rf.get("/")
        view = self._View()
        view.setup(request)
        assert view.get_success_url() == "/"
        assert view.get_redirect_url() == "/"
