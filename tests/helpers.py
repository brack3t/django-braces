from django import test
from django.contrib.auth.models import AnonymousUser, User, Permission
from django.core.serializers.json import DjangoJSONEncoder

from tests.models import UserObjectPermissions


class TestViewHelper:
    """
    Helper class for unit-testing class based views.
    """

    view_class = None
    request_factory_class = test.RequestFactory

    def setUp(self):
        super().setUp()
        self.factory = self.request_factory_class()

    def build_request(self, method="GET", path="/test/", user=None, **kwargs):
        """
        Creates a request using request factory.
        """
        fn = getattr(self.factory, method.lower())
        if user is None:
            user = AnonymousUser()

        req = fn(path, **kwargs)
        req.user = user
        return req

    def build_view(
        self, request, args=None, kwargs=None, view_class=None, **viewkwargs
    ):
        """
        Creates a `view_class` view instance.
        """
        if not args:
            args = ()
        if not kwargs:
            kwargs = {}
        if view_class is None:
            view_class = self.view_class

        return view_class(
            request=request, args=args, kwargs=kwargs, **viewkwargs
        )

    def dispatch_view(
        self, request, args=None, kwargs=None, view_class=None, **viewkwargs
    ):
        """
        Creates and dispatches `view_class` view.
        """
        view = self.build_view(request, args, kwargs, view_class, **viewkwargs)
        return view.dispatch(request, *view.args, **view.kwargs)


class SetJSONEncoder(DjangoJSONEncoder):
    """
    A custom JSONEncoder extending `DjangoJSONEncoder` to handle serialization
    of `set`.
    """

    def default(self, obj):
        """Control default methods of encoding data"""
        if isinstance(obj, set):
            return list(obj)
        return super(DjangoJSONEncoder, self).default(obj)


class PermissionChecker:
    """
    Custom Permission checker for testing of Object Level Permissions
    """
    def __init__(self, user: User):
        self.user = user

    def has_perm(self, perm: str, obj=None) -> bool:
        """This function is used to check for object level permissions"""
        if self.user and not self.user.is_active:
            return False
        elif self.user and self.user.is_superuser:
            return True
        if "." in perm:
            perm = perm.split(".", maxsplit=1)[1]
        permission_obj = Permission.objects.get(codename=perm)
        if obj is None:
            return perm in self.user.get_all_permissions(perm)
        return UserObjectPermissions.objects.filter(
            permission=permission_obj,
            user=self.user,
            article_object=obj
        ).exists()

    def has_perms(self, perms: list[str], obj=None) -> bool:
        """This function is used to check for object level permissions"""
        if self.user and not self.user.is_active:
            return False
        elif self.user and self.user.is_superuser:
            return True
        if not perms:
            return False
        return all(self.has_perm(perm) for perm in perms)

