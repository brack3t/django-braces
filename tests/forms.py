from __future__ import absolute_import

from django import forms

from braces.forms import UserKwargModelFormMixin

from .models import Article


class FormWithUserKwarg(UserKwargModelFormMixin, forms.Form):
    field1 = forms.CharField()


class ArticleForm(forms.ModelForm):
    class Meta:
        model = Article
        fields = ['author', 'title', 'body', 'slug']
