from django.contrib.auth.models import Permission, User
from django.db import models


class Article(models.Model):
    """
    A small but useful model for testing most features
    """
    author = models.ForeignKey(
        "auth.User", null=True, blank=True, on_delete=models.CASCADE
    )
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)


class CanonicalArticle(models.Model):
    """
    Model specifically for testing the canonical slug mixins
    """
    author = models.ForeignKey(
        "auth.User", null=True, blank=True, on_delete=models.CASCADE
    )
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)

    def get_canonical_slug(self):
        """Required by mixin to use the model as the source of truth"""
        if self.author:
            return f"{self.author.username}-{self.slug}"
        return f"unauthored-{self.slug}"


class UserObjectPermissions(models.Model):
    """Django model used to test and assign object level permissions"""
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    permission = models.ForeignKey(Permission, on_delete=models.CASCADE)
    article_object = models.ForeignKey(Article, on_delete=models.CASCADE)

