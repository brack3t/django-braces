from django.db import models


class Article(models.Model):
    author = models.ForeignKey('auth.User', null=True, blank=True)
    title = models.CharField(max_length=30)
    body = models.TextField()
    owner = models.ForeignKey('auth.User', null=True, blank=True,
                              related_name='articles_owned')
