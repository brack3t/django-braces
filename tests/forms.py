from django import forms
from braces.forms import UserKwargModelFormMixin


class FormWithUserKwarg(UserKwargModelFormMixin, forms.Form):
    field1 = forms.CharField()
