import six
import warnings

from django.conf import settings
from django.contrib import messages
from django.contrib.auth import REDIRECT_FIELD_NAME
from django.contrib.auth.views import redirect_to_login
from django.core import serializers
from django.core.exceptions import ImproperlyConfigured, PermissionDenied
from django.core.serializers.json import DjangoJSONEncoder
from django.core.urlresolvers import reverse
from django.http import HttpResponse
from django.utils.decorators import method_decorator
from django.utils.encoding import force_text
from django.views.generic import CreateView
from django.views.decorators.csrf import csrf_exempt

## Django 1.5+ compat
try:
    import json
except ImportError:  # pragma: no cover
    from django.utils import simplejson as json


class CreateAndRedirectToEditView(CreateView):
    """
    Subclass of CreateView which redirects to the edit view.
    Requires property `success_url_name` to be set to a
    reversible url that uses the objects pk.
    """
    success_url_name = None

    def dispatch(self, request, *args, **kwargs):
        warnings.warn(
            "CreateAndRedirectToEditView is deprecated and will be "
            "removed in a future release.", PendingDeprecationWarning)
        return super(CreateAndRedirectToEditView, self).dispatch(
            request, *args, **kwargs)

    def get_success_url(self):
        # First we check for a name to be provided on the view object.
        # If one is, we reverse it and finish running the method,
        # otherwise we raise a configuration error.
        if self.success_url_name:
            self.success_url = reverse(
                self.success_url_name, kwargs={'pk': self.object.pk})
            return super(CreateAndRedirectToEditView, self).get_success_url()

        raise ImproperlyConfigured(
            "No URL to reverse. Provide a success_url_name.")


class AccessMixin(object):
    """
    'Abstract' mixin that gives access mixins the same customizable
    functionality.
    """
    login_url = settings.LOGIN_URL  # LOGIN_URL from project settings
    raise_exception = False  # Default whether to raise an exception to none
    redirect_field_name = REDIRECT_FIELD_NAME  # Set by django.contrib.auth

    def get_login_url(self):
        """
        Override this method to customize the login_url.
        """
        if self.login_url is None:
            raise ImproperlyConfigured(
                "%(cls)s is missing the login_url. "
                "Define %(cls)s.login_url or override "
                "%(cls)s.get_login_url()." % {"cls": self.__class__.__name__})

        return force_text(self.login_url)

    def get_redirect_field_name(self):
        """
        Override this method to customize the redirect_field_name.
        """
        if self.redirect_field_name is None:
            raise ImproperlyConfigured(
                "%(cls)s is missing the "
                "redirect_field_name. Define %(cls)s.redirect_field_name or "
                "override %(cls)s.get_redirect_field_name()." % {
                "cls": self.__class__.__name__})

        return self.redirect_field_name


class LoginRequiredMixin(AccessMixin):
    """
    View mixin which verifies that the user is authenticated.

    NOTE:
        This should be the left-most mixin of a view, except when
        combined with CsrfExemptMixin - which in that case should
        be the left-most mixin.
    """
    def dispatch(self, request, *args, **kwargs):
        if not request.user.is_authenticated():
            if self.raise_exception:
                raise PermissionDenied  # return a forbidden response
            else:
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        return super(LoginRequiredMixin, self).dispatch(
            request, *args, **kwargs)


class CsrfExemptMixin(object):
    """
    Exempts the view from CSRF requirements.

    NOTE:
        This should be the left-most mixin of a view.
    """

    @method_decorator(csrf_exempt)
    def dispatch(self, *args, **kwargs):
        return super(CsrfExemptMixin, self).dispatch(*args, **kwargs)


class PermissionRequiredMixin(AccessMixin):
    """
    View mixin which verifies that the logged in user has the specified
    permission.

    Class Settings
    `permission_required` - the permission to check for.
    `login_url` - the login url of site
    `redirect_field_name` - defaults to "next"
    `raise_exception` - defaults to False - raise 403 if set to True

    Example Usage

        class SomeView(PermissionRequiredMixin, ListView):
            ...
            # required
            permission_required = "app.permission"

            # optional
            login_url = "/signup/"
            redirect_field_name = "hollaback"
            raise_exception = True
            ...
    """
    permission_required = None  # Default required perms to none

    def dispatch(self, request, *args, **kwargs):
        # Make sure that the permission_required attribute is set on the
        # view, or raise a configuration error.
        if self.permission_required is None:
            raise ImproperlyConfigured(
                "'PermissionRequiredMixin' requires "
                "'permission_required' attribute to be set.")

        # Check to see if the request's user has the required permission.
        has_permission = request.user.has_perm(self.permission_required)

        if not has_permission:  # If the user lacks the permission
            if self.raise_exception:  # *and* if an exception was desired
                raise PermissionDenied  # return a forbidden response.
            else:
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        return super(PermissionRequiredMixin, self).dispatch(
            request, *args, **kwargs)


class MultiplePermissionsRequiredMixin(AccessMixin):
    """
    View mixin which allows you to specify two types of permission
    requirements. The `permissions` attribute must be a dict which
    specifies two keys, `all` and `any`. You can use either one on
    its own or combine them. The value of each key is required to be a
    list or tuple of permissions. The standard Django permissions
    style is not strictly enforced. If you have created your own
    permissions in a different format, they should still work.

    By specifying the `all` key, the user must have all of
    the permissions in the passed in list.

    By specifying The `any` key , the user must have ONE of the set
    permissions in the list.

    Class Settings
        `permissions` - This is required to be a dict with one or both
            keys of `all` and/or `any` containing a list or tuple of
            permissions.
        `login_url` - the login url of site
        `redirect_field_name` - defaults to "next"
        `raise_exception` - defaults to False - raise 403 if set to True

    Example Usage
        class SomeView(MultiplePermissionsRequiredMixin, ListView):
            ...
            #required
            permissions = {
                "all": ("blog.add_post", "blog.change_post"),
                "any": ("blog.delete_post", "user.change_user")
            }

            #optional
            login_url = "/signup/"
            redirect_field_name = "hollaback"
            raise_exception = True
    """
    permissions = None  # Default required perms to none

    def dispatch(self, request, *args, **kwargs):
        self._check_permissions_attr()

        perms_all = self.permissions.get('all') or None
        perms_any = self.permissions.get('any') or None

        self._check_permissions_keys_set(perms_all, perms_any)
        self._check_perms_keys("all", perms_all)
        self._check_perms_keys("any", perms_any)

        # If perms_all, check that user has all permissions in the list/tuple
        if perms_all:
            if not request.user.has_perms(perms_all):
                if self.raise_exception:
                    raise PermissionDenied
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        # If perms_any, check that user has at least one in the list/tuple
        if perms_any:
            has_one_perm = False
            for perm in perms_any:
                if request.user.has_perm(perm):
                    has_one_perm = True
                    break

            if not has_one_perm:
                if self.raise_exception:
                    raise PermissionDenied
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        return super(MultiplePermissionsRequiredMixin, self).dispatch(
            request, *args, **kwargs)

    def _check_permissions_attr(self):
        """
        Check permissions attribute is set and that it is a dict.
        """
        if self.permissions is None or not isinstance(self.permissions, dict):
            raise ImproperlyConfigured(
                "'PermissionsRequiredMixin' requires "
                "'permissions' attribute to be set to a dict.")

    def _check_permissions_keys_set(self, perms_all=None, perms_any=None):
        """
        Check to make sure the keys `any` or `all` are not both blank.
        If both are blank either an empty dict came in or the wrong keys
        came in. Both are invalid and should raise an exception.
        """
        if perms_all is None and perms_any is None:
            raise ImproperlyConfigured(
                "'PermissionsRequiredMixin' requires"
                "'permissions' attribute to be set to a dict and the 'any' "
                "or 'all' key to be set.")

    def _check_perms_keys(self, key=None, perms=None):
        """
        If the permissions list/tuple passed in is set, check to make
        sure that it is of the type list or tuple.
        """
        if perms and not isinstance(perms, (list, tuple)):
            raise ImproperlyConfigured(
                "'MultiplePermissionsRequiredMixin' "
                "requires permissions dict '%s' value to be a list "
                "or tuple." % key)


class GroupRequiredMixin(AccessMixin):
    group_required = None

    def get_group_required(self):
        if self.group_required is None or (
                not isinstance(self.group_required,
                               (list, tuple) + six.string_types)):

            raise ImproperlyConfigured(
                "'GroupRequiredMixin' requires "
                "'group_required' attribute to be set and be one of the "
                "following types: string, unicode, list, or tuple.")
        return self.group_required

    def check_membership(self, group):
        """ Check required group(s) """
        if not group in self.request.user.groups.values_list('name',
                                                             flat=True):
            return False
        return True

    def dispatch(self, request, *args, **kwargs):
        in_group = self.check_membership(self.get_group_required())

        if not in_group:
            if self.raise_exception:
                raise PermissionDenied
            else:
                return redirect_to_login(
                    request.get_full_path(),
                    self.get_login_url(),
                    self.get_redirect_field_name())
        return super(GroupRequiredMixin, self).dispatch(
            request, *args, **kwargs)


class UserFormKwargsMixin(object):
    """
    CBV mixin which puts the user from the request into the form kwargs.
    Note: Using this mixin requires you to pop the `user` kwarg
    out of the dict in the super of your form's `__init__`.
    """
    def get_form_kwargs(self):
        kwargs = super(UserFormKwargsMixin, self).get_form_kwargs()
        # Update the existing form kwargs dict with the request's user.
        kwargs.update({"user": self.request.user})
        return kwargs


class SuccessURLRedirectListMixin(object):
    """
    Simple CBV mixin which sets the success url to the list view of
    a given app. Set success_list_url as a class attribute of your
    CBV and don't worry about overloading the get_success_url.

    This is only to be used for redirecting to a list page. If you need
    to reverse the url with kwargs, this is not the mixin to use.
    """
    success_list_url = None  # Default the success url to none

    def get_success_url(self):
        # Return the reversed success url.
        if self.success_list_url is None:
            raise ImproperlyConfigured(
                "%(cls)s is missing a succes_list_url "
                "name to reverse and redirect to. Define "
                "%(cls)s.success_list_url or override "
                "%(cls)s.get_success_url()"
                "." % {"cls": self.__class__.__name__})
        return reverse(self.success_list_url)


class SuperuserRequiredMixin(AccessMixin):
    """
    Mixin allows you to require a user with `is_superuser` set to True.
    """
    def dispatch(self, request, *args, **kwargs):
        if not request.user.is_superuser:  # If the user is a standard user,
            if self.raise_exception:  # *and* if an exception was desired
                raise PermissionDenied  # return a forbidden response.
            else:
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        return super(SuperuserRequiredMixin, self).dispatch(
            request, *args, **kwargs)


class SetHeadlineMixin(object):
    """
    Mixin allows you to set a static headline through a static property on the
    class or programmatically by overloading the get_headline method.
    """
    headline = None  # Default the headline to none

    def get_context_data(self, **kwargs):
        kwargs = super(SetHeadlineMixin, self).get_context_data(**kwargs)
        # Update the existing context dict with the provided headline.
        kwargs.update({"headline": self.get_headline()})
        return kwargs

    def get_headline(self):
        if self.headline is None:  # If no headline was provided as a view
                                   # attribute and this method wasn't
                                   # overridden raise a configuration error.
            raise ImproperlyConfigured(
                "%(cls)s is missing a headline. "
                "Define %(cls)s.headline, or override "
                "%(cls)s.get_headline()." % {"cls": self.__class__.__name__}
            )
        return self.headline


class SelectRelatedMixin(object):
    """
    Mixin allows you to provide a tuple or list of related models to
    perform a select_related on.
    """
    select_related = None  # Default related fields to none

    def get_queryset(self):
        if self.select_related is None:  # If no fields were provided,
                                         # raise a configuration error
            raise ImproperlyConfigured(
                "%(cls)s is missing the "
                "select_related property. This must be a tuple or list." % {
                    "cls": self.__class__.__name__})

        if not isinstance(self.select_related, (tuple, list)):
            # If the select_related argument is *not* a tuple or list,
            # raise a configuration error.
            raise ImproperlyConfigured(
                "%(cls)s's select_related property "
                "must be a tuple or list." % {"cls": self.__class__.__name__})

        # Get the current queryset of the view
        queryset = super(SelectRelatedMixin, self).get_queryset()

        return queryset.select_related(*self.select_related)


class PrefetchRelatedMixin(object):
    """
    Mixin allows you to provide a tuple or list of related models to
    perform a prefetch_related on.
    """
    prefetch_related = None  # Default prefetch fields to none

    def get_queryset(self):
        if self.prefetch_related is None:  # If no fields were provided,
                                           # raise a configuration error
            raise ImproperlyConfigured(
                "%(cls)s is missing the "
                "prefetch_related property. This must be a tuple or list." % {
                    "cls": self.__class__.__name__})

        if not isinstance(self.prefetch_related, (tuple, list)):
            # If the select_related argument is *not* a tuple or list,
            # raise a configuration error.
            raise ImproperlyConfigured(
                "%(cls)s's prefetch_related property "
                "must be a tuple or list." % {"cls": self.__class__.__name__})

        # Get the current queryset of the view
        queryset = super(PrefetchRelatedMixin, self).get_queryset()

        return queryset.prefetch_related(*self.prefetch_related)


class StaffuserRequiredMixin(AccessMixin):
    """
    Mixin allows you to require a user with `is_staff` set to True.
    """
    def dispatch(self, request, *args, **kwargs):
        if not request.user.is_staff:  # If the request's user is not staff,
            if self.raise_exception:  # *and* if an exception was desired
                raise PermissionDenied  # return a forbidden response
            else:
                return redirect_to_login(request.get_full_path(),
                                         self.get_login_url(),
                                         self.get_redirect_field_name())

        return super(StaffuserRequiredMixin, self).dispatch(
            request, *args, **kwargs)


class JSONResponseMixin(object):
    """
    A mixin that allows you to easily serialize simple data such as a dict or
    Django models.
    """
    content_type = "application/json"
    json_dumps_kwargs = None

    def get_content_type(self):
        if self.content_type is None:
            raise ImproperlyConfigured(
                "%(cls)s is missing a content type. "
                "Define %(cls)s.content_type, or override "
                "%(cls)s.get_content_type()." % {
                "cls": self.__class__.__name__}
            )
        return self.content_type

    def get_json_dumps_kwargs(self):
        if self.json_dumps_kwargs is None:
            self.json_dumps_kwargs = {}
        self.json_dumps_kwargs.setdefault('ensure_ascii', False)
        return self.json_dumps_kwargs

    def render_json_response(self, context_dict, status=200):
        """
        Limited serialization for shipping plain data. Do not use for models
        or other complex or custom objects.
        """
        json_context = json.dumps(context_dict, cls=DjangoJSONEncoder,
                                  **self.get_json_dumps_kwargs())
        return HttpResponse(json_context,
                            content_type=self.get_content_type(),
                            status=status)

    def render_json_object_response(self, objects, **kwargs):
        """
        Serializes objects using Django's builtin JSON serializer. Additional
        kwargs can be used the same way for django.core.serializers.serialize.
        """
        json_data = serializers.serialize("json", objects, **kwargs)
        return HttpResponse(json_data, content_type=self.get_content_type())


class AjaxResponseMixin(object):
    """
    Mixin allows you to define alternative methods for ajax requests. Similar
    to the normal get, post, and put methods, you can use get_ajax, post_ajax,
    and put_ajax.
    """
    def dispatch(self, request, *args, **kwargs):
        request_method = request.method.lower()

        if request.is_ajax() and request_method in self.http_method_names:
            handler = getattr(self, '%s_ajax' % request_method,
                              self.http_method_not_allowed)
            self.request = request
            self.args = args
            self.kwargs = kwargs
            return handler(request, *args, **kwargs)

        return super(AjaxResponseMixin, self).dispatch(
            request, *args, **kwargs)

    def get_ajax(self, request, *args, **kwargs):
        return self.get(request, *args, **kwargs)

    def post_ajax(self, request, *args, **kwargs):
        return self.post(request, *args, **kwargs)

    def put_ajax(self, request, *args, **kwargs):
        return self.get(request, *args, **kwargs)

    def delete_ajax(self, request, *args, **kwargs):
        return self.get(request, *args, **kwargs)


class OrderableListMixin(object):
    """
    Mixin allows your users to order records using GET parameters
    """

    orderable_columns = None
    orderable_columns_default = None
    order_by = None
    ordering = None

    def get_context_data(self, **kwargs):
        """
        Augments context with:

            * ``order_by`` - name of the field
            * ``ordering`` - order of ordering, either ``asc`` or ``desc``
        """
        context = super(OrderableListMixin, self).get_context_data(**kwargs)
        context["order_by"] = self.order_by
        context["ordering"] = self.ordering
        return context

    def get_orderable_columns(self):
        if not self.orderable_columns:
            raise ImproperlyConfigured(
                "Please define allowed ordering columns")
        return self.orderable_columns

    def get_orderable_columns_default(self):
        if not self.orderable_columns_default:
            raise ImproperlyConfigured("Please define default ordering column")
        return self.orderable_columns_default

    def get_ordered_queryset(self, queryset=None):
        """
        Augments ``QuerySet`` with order_by statement if possible

        :param QuerySet queryset: ``QuerySet`` to ``order_by``
        :return: QuerySet
        """
        get_order_by = self.request.GET.get("order_by")

        if get_order_by in self.get_orderable_columns():
            order_by = get_order_by
        else:
            order_by = self.get_orderable_columns_default()

        self.order_by = order_by
        self.ordering = "asc"

        if order_by and self.request.GET.get("ordering", "asc") == "desc":
            order_by = "-" + order_by
            self.ordering = "desc"

        return queryset.order_by(order_by)

    def get_queryset(self):
        """
        Returns ordered ``QuerySet``
        """
        unordered_queryset = super(OrderableListMixin, self).get_queryset()
        return self.get_ordered_queryset(unordered_queryset)


class FormValidMessageMixin(object):
    """
    Mixin allows you to set static message which is displayed by
    Django's messages framework through a static property on the class
    or programmatically by overloading the get_form_valid_message method.
    """
    form_valid_message = None  # Default to None

    def get_form_valid_message(self):
        """
        Validate that form_valid_message is set and is either a
        unicode or str object.
        """
        if self.form_valid_message is None:
            raise ImproperlyConfigured(
                '{0}.form_valid_message is not set. Define '
                '{0}.form_valid_message, or override '
                '{0}.get_form_valid_message().'.format(self.__class__.__name__)
            )

        if not isinstance(self.form_valid_message, six.string_types):
            raise ImproperlyConfigured(
                '{0}.form_valid_message must be a str or unicode '
                'object.'.format(self.__class__.__name__)
            )

        return self.form_valid_message

    def form_valid(self, form):
        """
        Call the super first, so that when overriding
        get_form_valid_message, we have access to the newly saved object.
        """
        response = super(FormValidMessageMixin, self).form_valid(form)
        messages.success(self.request, self.get_form_valid_message(),
                         fail_silently=True)
        return response


class FormInvalidMessageMixin(object):
    """
    Mixin allows you to set static message which is displayed by
    Django's messages framework through a static property on the class
    or programmatically by overloading the get_form_invalid_message method.
    """
    form_invalid_message = None

    def get_form_invalid_message(self):
        """
        Validate that form_invalid_message is set and is either a
        unicode or str object.
        """
        if self.form_invalid_message is None:
            raise ImproperlyConfigured(
                '{0}.form_invalid_message is not set. Define '
                '{0}.form_invalid_message, or override '
                '{0}.get_form_invalid_message().'.format(
                    self.__class__.__name__)
            )

        if not isinstance(self.form_invalid_message, six.string_types):
            raise ImproperlyConfigured(
                '{0}.form_invalid_message must be a str or unicode '
                'object.'.format(self.__class__.__name__)
            )

        return self.form_invalid_message

    def form_invalid(self, form):
        response = super(FormInvalidMessageMixin, self).form_invalid(form)
        messages.error(self.request, self.get_form_invalid_message(),
                       fail_silently=True)
        return response


class FormMessagesMixin(FormValidMessageMixin, FormInvalidMessageMixin):
    """
    Mixin is a shortcut to use both FormValidMessageMixin and
    FormInvalidMessageMixin.
    """
    pass
