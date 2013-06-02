from django.db import models


class Article(models.Model):
    author = models.ForeignKey('auth.User', null=True, blank=True)
    title = models.CharField(max_length=30)
    body = models.TextField()


class ArticleWithHasOwner(models.Model):
    author = models.ForeignKey('auth.User', null=True, blank=True)
    title = models.CharField(max_length=30)
    body = models.TextField()
    owner_result = models.BooleanField()

    def has_owner(self, user):
        return self.owner_result