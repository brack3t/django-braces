from django.db import models


class Article(models.Model):
    author = models.ForeignKey('auth.User', null=True, blank=True)
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)


class CanonicalArticle(models.Model):
    author = models.ForeignKey('auth.User', null=True, blank=True)
    title = models.CharField(max_length=30)
    body = models.TextField()
    slug = models.SlugField(blank=True)

    def get_canonical_slug(self):
        if self.author:
            return "{}-{}".format(self.author.username, self.slug)
        return "unauthored-{}".format(self.slug)

