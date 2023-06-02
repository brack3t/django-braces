"""Tests relating to the form mixins."""
import pytest
from django import forms
from django.core.exceptions import ImproperlyConfigured
from pytest_lazyfixture import lazy_fixture as lazy

from braces import mixins


class TestUserFormMixin:
    """Tests for the UserFormMixin."""

    class InvalidForm(mixins.UserFormMixin):
        """A non-form form."""

    class ValidForm(mixins.UserFormMixin, forms.Form):
        """A form form."""

    def test_invalid_class(self):
        """Invalid forms raise an exception."""
        with pytest.raises(TypeError):
            self.InvalidForm()

    def test_form_has_user(self, admin_user):
        """Valid forms contain the user."""
        form = self.ValidForm(user=admin_user)
        assert form.user == admin_user


@pytest.mark.mixin("FormWithUserMixin")
class TestFormWithUserMixin:
    """Tests related to the `FormWithUserMixin`."""

    def test_user_to_form_kwargs(self, form_view, admin_user, rf):
        """User should appear in form's kwargs."""
        request = rf.get("/")
        request.user = admin_user
        view = form_view()
        form_kwargs = view(request=request).get_form_kwargs()
        assert form_kwargs["user"] == admin_user

    def test_user_to_form_wrapped_class(self, form_view, form_class, admin_user, rf):
        """A non-UserFormMixin form has the mixin applied."""
        request = rf.get("/")
        request.user = admin_user
        view = form_view()
        view.form_class = form_class()
        assert issubclass(view(request=request).get_form_class(), mixins.UserFormMixin)


@pytest.mark.parametrize(
    ("form", "view"),
    [
        (lazy("form_class"), lazy("form_view")),
        (lazy("model_form_class"), lazy("model_form_view")),
    ],
)
@pytest.mark.mixin("CSRFExemptMixin")
class TestCSRFExempt:
    """Tests for the CSRFExemptMixin."""

    def test_csrf_exempt(self, form, view, rf):
        """CSRF-exempt views should pass without a CSRF token."""
        view = view()
        view.form_class = form()
        view.success_url = "/"

        request = rf.post("/")
        view().setup(request=request)
        assert view().dispatch(request=request).status_code == 200


@pytest.mark.mixin("MultipleFormsMixin")
class TestMultipleFormsMixin:
    """Tests related to the MultipleFormsMixin."""

    def test_missing_attribute(self, form_view):
        """A view with no instances or initials should fail."""
        view_class = form_view()
        with pytest.raises(ImproperlyConfigured):
            view_class().get_form_classes()

    @pytest.mark.parametrize(
        ("form", "view"),
        [
            (lazy("form_class"), lazy("form_view")),
            (lazy("model_form_class"), lazy("model_form_view")),
        ],
    )
    def test_form_classes(self, form, view):
        """The view should return all prescribed form classes."""
        fv = view()
        fc = form()
        fv.form_classes = {"two": fc, "one": fc}
        assert fv().get_form_classes() == {"one": fc, "two": fc}

    @pytest.mark.parametrize(
        ("form", "view"),
        [
            (lazy("form_class"), lazy("form_view")),
            (lazy("model_form_class"), lazy("model_form_view")),
        ],
    )
    def test_forms_in_context(self, form, view, rf):
        """Forms should appear in the view's context."""
        req = rf.get("/")
        view = view()
        view.form_classes = {"one": form(), "two": form()}
        context = view(request=req).get_context_data()
        assert "one" in context["forms"]
        assert "two" in context["forms"]

    @pytest.mark.parametrize(
        ("form", "view"),
        [
            (lazy("form_class"), lazy("form_view")),
            (lazy("model_form_class"), lazy("model_form_view")),
        ],
    )
    def test_forms_with_initial_values(self, form, view, rf):
        """Initial values provided in the view should show in the form."""
        request = rf.get("/")
        view = view()
        view.initials = {"one": {"name": "bar"}}
        view.form_classes = {"one": form(), "two": form()}
        context = view(request=request).get_context_data()
        assert context["forms"]["one"].initial == {"name": "bar"}
        assert context["forms"]["two"].initial == {}

    @pytest.mark.parametrize(
        ("form", "view"),
        [
            (lazy("form_class"), lazy("form_view")),
            (lazy("model_form_class"), lazy("model_form_view")),
        ],
    )
    def test_forms_valid(self, form, view, rf):
        """Forms receiving valid data should validate as such."""
        request = rf.post("/", data={"one-title": "foo", "two-slug": 42})
        view = view()
        view.form_classes = {
            "one": form(title=forms.CharField()),
            "two": form(slug=forms.SlugField()),
        }
        context = view(request=request).get_context_data()
        assert context["forms"]["one"].is_valid()
        assert context["forms"]["two"].is_valid()
        assert view(request=request).validate_forms()

    @pytest.mark.parametrize(
        ("view", "valid"),
        [
            (lazy("form_view"), True),
            (lazy("form_view"), False),
            (lazy("model_form_view"), True),
            (lazy("model_form_view"), False),
        ],
    )
    def test_not_implemented(self, view, valid):
        """Views much implement `forms_valid` and `forms_invalid`."""
        view = view()()
        method = "forms_valid" if valid else "forms_invalid"
        with pytest.raises(NotImplementedError):
            getattr(view, method)()
