from django import forms

from braces import mixins

from .models import Article


class FormWithUserKwarg(mixins.UserFormMixin, forms.Form):
    """This form will get a `user` kwarg"""

    field1 = forms.CharField()


class ArticleForm(forms.ModelForm):
    """This form represents an Article"""

    class Meta:
        model = Article
        fields = ["author", "title", "body", "slug"]
