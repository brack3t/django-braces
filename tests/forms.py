from django import forms

from braces.forms import UserKwargModelFormMixin

from .models import Article


class FormWithUserKwarg(UserKwargModelFormMixin, forms.Form):
    """This form will get a `user` kwarg"""
    field1 = forms.CharField()


class ArticleForm(forms.ModelForm):
    """This form represents an Article"""
    class Meta:
        model = Article
        fields = ["author", "title", "body", "slug"]
