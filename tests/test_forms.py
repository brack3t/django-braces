import pytest
from django import forms
from django.core.exceptions import ImproperlyConfigured
from django.views.generic import FormView

from braces import mixins
from .project.models import Article, CanonicalArticle



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


class TestMultipleFormsMixin:
    def setup_method(self):
        self.Form1 = type("Form1", (forms.Form,), {"name": forms.CharField()})
        self.Form2 = type("Form2", (forms.Form,), {"age": forms.IntegerField()})

        class _View(mixins.MultipleFormsMixin, FormView):
            form_classes = {
                "my_form": self.Form1,
                "my_form2": self.Form2
            }
            template_name = "test.html"

        self._View = _View

    def test_missing_attribute(self):
        class InvalidForm(mixins.MultipleFormsMixin, FormView):
            pass

        with pytest.raises(ImproperlyConfigured):
            InvalidForm().get_form_classes()

    def test_form_classes(self):
        assert self._View().get_form_classes() == {"my_form": self.Form1, "my_form2": self.Form2}

    def test_forms_in_context(self, rf):
        request = rf.get("/")
        view = self._View()
        view.setup(request)
        view.get(request)
        assert "my_form" in view.get_context_data()["forms"]
        assert "my_form2" in view.get_context_data()["forms"]

    def test_forms_with_initial_values(self, rf):
        request = rf.get("/")
        view = self._View()
        view.initial = {"my_form": {"name": "bar"}}
        view.setup(request)
        view.get(request)
        assert view.get_context_data()["forms"]["my_form"].initial == {"name": "bar"}
        assert view.get_context_data()["forms"]["my_form2"].initial == {}

    def test_forms_valid(self, rf):
        request = rf.post("/", data={"my_form-name": "foo", "my_form2-age": 42})
        view = self._View()
        view.setup(request)
        assert view.get_context_data()["forms"]["my_form"].is_valid()
        assert view.get_context_data()["forms"]["my_form2"].is_valid()
        assert view.validate_forms()

    def test_not_implemented(self):
        view = self._View()
        with pytest.raises(NotImplementedError):
            view.forms_valid()

        with pytest.raises(NotImplementedError):
            view.forms_invalid()


class TestMultipleModelFormsMixin:
    class _Form1(forms.ModelForm):
        class Meta:
            model = Article
            fields = ["title"]

    class _Form2(forms.ModelForm):
        class Meta:
            model = CanonicalArticle
            fields = ["title"]

    def setup_method(self):
        article = Article.objects.create(title="foo")
        class _View(mixins.MultipleModelFormsMixin, FormView):
            form_classes = {
                "my_form": self._Form1,
                "my_form2": self._Form2
            }
            instances = {"my_form": article}
            template_name = "test.html"

        self._View = _View

    @pytest.mark.django_db
    def test_get_instances(self):
        assert self._View().get_instances() == {
            "my_form": Article.objects.get(title="foo")
        }
