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
