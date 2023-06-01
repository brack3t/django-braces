"""Article and CanonicalArticle models for the django-braces project."""
from __future__ import annotations

from django.db import models


class Article(models.Model):
    """A small but useful model for testing most features."""

    author = models.ForeignKey(
        "auth.User", null=True, blank=True, on_delete=models.CASCADE
    )
    coauthor = models.ForeignKey(
        "auth.User", null=True, blank=True, on_delete=models.CASCADE
    )
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)

    class Meta:
        app_label = "project"

    def __str__(self):
        return f"_{self.title}_ by {self.author__name}."


class CanonicalArticle(models.Model):
    """Model specifically for testing the canonical slug mixins."""

    author = models.ForeignKey(
        "auth.User", null=True, blank=True, on_delete=models.CASCADE
    )
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)

    class Meta:
        app_label = "project"

    def get_canonical_slug(self) -> str:
        """Return the slug of record."""
        if self.author:
            return f"{self.author.username}-{self.slug}"
        return f"unauthored-{self.slug}"

    def __str__(self):
        return f"_{self.title}_ by {self.author__name}."
