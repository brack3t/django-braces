from __future__ import absolute_import

import factory

from django.contrib.auth.models import Group, Permission, User

from .models import Article


def _get_perm(perm_name):
    """
    Returns permission instance with given name.

    Permission name is a string like 'auth.add_user'.
    """
    app_label, codename = perm_name.split('.')
    return Permission.objects.get(
        content_type__app_label=app_label, codename=codename)


class ArticleFactory(factory.django.DjangoModelFactory):
    title = factory.Sequence(lambda n: 'Article number {0}'.format(n))
    body = factory.Sequence(lambda n: 'Body of article {0}'.format(n))

    class Meta:
        model = Article
        abstract = False


class GroupFactory(factory.django.DjangoModelFactory):
    name = factory.Sequence(lambda n: 'group{0}'.format(n))

    class Meta:
        model = Group
        abstract = False


class UserFactory(factory.django.DjangoModelFactory):
    username = factory.Sequence(lambda n: 'user{0}'.format(n))
    first_name = factory.Sequence(lambda n: 'John {0}'.format(n))
    last_name = factory.Sequence(lambda n: 'Doe {0}'.format(n))
    email = factory.Sequence(lambda n: 'user{0}@example.com'.format(n))
    password = 'asdf1234'

    class Meta:
        model = User
        abstract = False

    @classmethod
    def _prepare(cls, create, **kwargs):
        password = kwargs.pop('password', None)
        user = super(UserFactory, cls)._prepare(create, **kwargs)
        if password:
            user.set_password(password)
            if create:
                user.save()
        return user

    @factory.post_generation
    def permissions(self, create, extracted, **kwargs):
        if create and extracted:
            # We have a saved object and a list of permission names
            self.user_permissions.add(*[_get_perm(pn) for pn in extracted])
