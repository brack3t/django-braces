import factory

from django.contrib.auth.models import Group, Permission, User
from django.contrib.contenttypes.models import ContentType

from .models import Article, UserObjectPermissions


def _get_perm(perm_name):
    """
    Returns permission instance with given name.

    Permission name is a string like 'auth.add_user'.
    """
    app_label, codename = perm_name.split(".")
    return Permission.objects.get(
        content_type__app_label=app_label, codename=codename
    )


class ArticleFactory(factory.django.DjangoModelFactory):
    """Generates Articles"""
    title = factory.Sequence(lambda n: f"Article number {n}")
    body = factory.Sequence(lambda n: "Body of article {n}")

    class Meta:
        model = Article
        abstract = False


class GroupFactory(factory.django.DjangoModelFactory):
    """Artificial divides as a service"""
    name = factory.Sequence(lambda n: f"group{n}")

    class Meta:
        model = Group
        abstract = False


class UserFactory(factory.django.DjangoModelFactory):
    """The people who make it all possible"""
    username = factory.Sequence(lambda n: f"user{n}")
    first_name = factory.Sequence(lambda n: f"John {n}")
    last_name = factory.Sequence(lambda n: f"Doe {n}")
    email = factory.Sequence(lambda n: f"user{n}@example.com")
    password = factory.PostGenerationMethodCall("set_password", "asdf1234")

    class Meta:
        model = User
        abstract = False

    @factory.post_generation
    def permissions(self, create, extracted, **kwargs):
        """Give the user some permissions"""
        if create and extracted:
            # We have a saved object and a list of permission names
            self.user_permissions.add(*[_get_perm(pn) for pn in extracted])


class ContentTypeFactory(factory.django.DjangoModelFactory):
    """Factory for creating `ContentType` model objects"""
    app_label = factory.Sequence(lambda n: f"app_label_{n}")
    model = factory.Sequence(lambda n: f"model_{n}")

    class Meta:
        model = ContentType
        abstract = False


class PermissionFactory(factory.django.DjangoModelFactory):
    """Factory for creating `Permission` model objects"""
    name = factory.Sequence(lambda n: f"name_{n}")
    codename = factory.Sequence(lambda n: f"codename_{n}")
    content_type = factory.SubFactory(ContentTypeFactory)

    class Meta:
        model = Permission
        abstract = False


class UserObjectPermissionsFactory(factory.django.DjangoModelFactory):
    """Factory for creating `UserObjectPermissions` model objects"""
    user = factory.SubFactory(UserFactory)
    permission = factory.SubFactory(PermissionFactory)
    article_object = factory.SubFactory(ArticleFactory)

    class Meta:
        model = UserObjectPermissions
        abstract = False
