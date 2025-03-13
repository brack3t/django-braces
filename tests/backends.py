"""Testing backend for object level permissions"""
from tests.helpers import PermissionChecker


class PermissionsCheckerBackend:
    """
    Custom Permission Backend for testing Object Level Permissions.
    """
    supports_object_permissions = True
    supports_anonymous_user = True
    supports_inactive_user = True

    @staticmethod
    def authenticate():
        """Required for a backend"""
        return None

    @staticmethod
    def has_perm(user_obj, perm, obj=None):
        """Used for checking permissions using the `PermissionChecker`"""
        check = PermissionChecker(user_obj)
        return check.has_perm(perm, obj)

    @staticmethod
    def has_perms(user_obj, perms: list[str], obj=None):
        """Used for checking multiple permissions using the `PermissionChecker`"""
        check = PermissionChecker(user_obj)
        return check.has_perms(perms, obj)
