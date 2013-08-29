from django.contrib.auth.models import User, Permission
from .models import Article

_i = 0


def get_next_id():
    """Returns unique integer."""
    global _i
    ret = _i
    _i += 1
    return ret


def _get_perm(perm_name):
    """
    Returns permission instance with given name.

    Permission name is a string like 'auth.add_user'.
    """
    app_label, codename = perm_name.split('.')
    return Permission.objects.get(
        content_type__app_label=app_label, codename=codename)


def make_user(permissions=None, password='asdf1234', **kwargs):
    """
    Creates new user instance.

    `permissions` is a list of permission names like ['auth.add_user'].
    `password` is raw (not hashed) password. It defaults to 'asdf1234'.
    """
    i = get_next_id()
    defaults = {'username': 'user%s' % i, 'first_name': 'John %s' % i,
                'last_name': 'Doe %s' % i, 'email': 'user%s@example.com' % i}
    defaults.update(**kwargs)
    obj = User(**defaults)
    obj.set_password(password)
    obj.save()
    if permissions:
        obj.user_permissions.add(*[_get_perm(pn) for pn in permissions])
    return obj


def make_article(**kwargs):
    i = get_next_id()
    defaults = {'title': "Article number %s" % i,
                'body': "Body of article %s" % i,
                'owner': None}
    if 'user' in kwargs:
        user = kwargs.pop('user')
    if 'set_owner' in kwargs and kwargs.pop('set_owner'):
        kwargs['owner'] = user

    defaults.update(kwargs)
    return Article.objects.create(**defaults)
