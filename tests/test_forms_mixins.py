from django import test
from django.contrib.auth.models import User

from . import forms


class TestUserKwargModelFormMixin(test.TestCase):
    """
    Tests for UserKwargModelFormMixin.
    """

    def test_without_user_kwarg(self):
        """
        It should be possible to create form without 'user' kwarg.

        In that case 'user' attribute should be set to None.
        """
        form = forms.FormWithUserKwarg()
        assert form.user is None

    def test_with_user_kwarg(self):
        """
        Form's 'user' attribute should be set to value passed as 'user'
        argument.
        """
        user = User(username="test")
        form = forms.FormWithUserKwarg(user=user)
        assert form.user is user
