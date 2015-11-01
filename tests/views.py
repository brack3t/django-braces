from __future__ import absolute_import

import codecs

from django.contrib.auth.models import User
from django.http import HttpResponse
from django.utils.translation import ugettext_lazy as _
from django.views.generic import (View, UpdateView, FormView, TemplateView,
                                  ListView, DetailView, CreateView)

from braces import views

from .models import Article, CanonicalArticle
from .forms import ArticleForm, FormWithUserKwarg
from .helpers import SetJSONEncoder


class OkView(View):
    """
    A view which simply returns "OK" for every request.
    """
    def get(self, request):
        return HttpResponse("OK")

    def post(self, request):
        return self.get(request)

    def put(self, request):
        return self.get(request)

    def delete(self, request):
        return self.get(request)


class LoginRequiredView(views.LoginRequiredMixin, OkView):
    """
    A view for testing LoginRequiredMixin.
    """


class AnonymousRequiredView(views.AnonymousRequiredMixin, OkView):
    """
    A view for testing AnonymousRequiredMixin. Should accept
    unauthenticated users and redirect authenticated users to the
    authenticated_redirect_url set on the view.
    """
    authenticated_redirect_url = '/authenticated_view/'


class AuthenticatedView(views.LoginRequiredMixin, OkView):
    """
    A view for testing AnonymousRequiredMixin. Should accept
    authenticated users.
    """


class AjaxResponseView(views.AjaxResponseMixin, OkView):
    """
    A view for testing AjaxResponseMixin.
    """
    def get_ajax(self, request):
        return HttpResponse("AJAX_OK")

    def post_ajax(self, request):
        return self.get_ajax(request)

    def put_ajax(self, request):
        return self.get_ajax(request)

    def delete_ajax(self, request):
        return self.get_ajax(request)


class SimpleJsonView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_response() method.
    """
    def get(self, request):
        object = {'username': request.user.username}
        return self.render_json_response(object)


class CustomJsonEncoderView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's `json_encoder_class` attribute
    with custom JSONEncoder class.
    """
    json_encoder_class = SetJSONEncoder

    def get(self, request):
        object = {'numbers': set([1, 2, 3])}
        return self.render_json_response(object)


class SimpleJsonBadRequestView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_response() method with
    400 HTTP status code.
    """
    def get(self, request):
        object = {'username': request.user.username}
        return self.render_json_response(object, status=400)


class ArticleListJsonView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_object_response()
    method.
    """
    def get(self, request):
        queryset = Article.objects.all()
        return self.render_json_object_response(
            queryset, fields=('title',))


class JsonRequestResponseView(views.JsonRequestResponseMixin, View):
    """
    A view for testing JsonRequestResponseMixin's json conversion
    """
    def post(self, request):
        return self.render_json_response(self.request_json)


class JsonBadRequestView(views.JsonRequestResponseMixin, View):
    """
    A view for testing JsonRequestResponseMixin's require_json
    and render_bad_request_response methods
    """
    require_json = True

    def post(self, request, *args, **kwargs):
        return self.render_json_response(self.request_json)


class JsonCustomBadRequestView(views.JsonRequestResponseMixin, View):
    """
    A view for testing JsonRequestResponseMixin's
    render_bad_request_response method with a custom error message
    """
    def post(self, request, *args, **kwargs):
        if not self.request_json:
            return self.render_bad_request_response(
                {'error': 'you messed up'})
        return self.render_json_response(self.request_json)


class CreateArticleView(CreateView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    fields = ['author', 'title', 'body', 'slug']
    model = Article
    template_name = 'form.html'


class EditArticleView(UpdateView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    model = Article
    template_name = 'form.html'


class CreateArticleAndRedirectToListView(views.SuccessURLRedirectListMixin,
                                         CreateArticleView):
    """
    View for testing SuccessURLRedirectListMixin
    """
    success_list_url = 'article_list'


class CreateArticleAndRedirectToListViewBad(views.SuccessURLRedirectListMixin,
                                            CreateArticleView):
    """
    View for testing SuccessURLRedirectListMixin
    """
    success_list_url = None


class ArticleListView(views.SelectRelatedMixin, ListView):
    """
    A list view for articles, required for testing SuccessURLRedirectListMixin.

    Also used to test SelectRelatedMixin.
    """
    model = Article
    template_name = 'blank.html'
    select_related = ('author',)


class ArticleListViewWithCustomQueryset(views.SelectRelatedMixin, ListView):
    """
    Another list view for articles, required to test SelectRelatedMixin.
    """
    queryset = Article.objects.select_related('author').prefetch_related(
        'article_set')
    template_name = 'blank.html'
    select_related = ()


class FormWithUserKwargView(views.UserFormKwargsMixin, FormView):
    """
    View for testing UserFormKwargsMixin.
    """
    form_class = FormWithUserKwarg
    template_name = 'form.html'

    def form_valid(self, form):
        return HttpResponse("username: %s" % form.user.username)


class HeadlineView(views.SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin.
    """
    template_name = 'blank.html'
    headline = "Test headline"


class LazyHeadlineView(views.SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin.
    """
    template_name = 'blank.html'
    headline = _("Test Headline")


class ContextView(views.StaticContextMixin, TemplateView):
    """ View for testing StaticContextMixin. """
    template_name = 'blank.html'
    static_context = {'test': True}


class DynamicHeadlineView(views.SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin's get_headline() method.
    """
    template_name = 'blank.html'

    def get_headline(self):
        return self.kwargs['s']


class PermissionRequiredView(views.PermissionRequiredMixin, OkView):
    """
    View for testing PermissionRequiredMixin.
    """
    permission_required = 'auth.add_user'


class MultiplePermissionsRequiredView(views.MultiplePermissionsRequiredMixin,
                                      OkView):
    permissions = {
        'all': ['tests.add_article', 'tests.change_article'],
        'any': ['auth.add_user', 'auth.change_user'],
    }


class SuperuserRequiredView(views.SuperuserRequiredMixin, OkView):
    pass


class StaffuserRequiredView(views.StaffuserRequiredMixin, OkView):
    pass


class CsrfExemptView(views.CsrfExemptMixin, OkView):
    pass


class AuthorDetailView(views.PrefetchRelatedMixin, ListView):
    model = User
    prefetch_related = ['article_set']
    template_name = 'blank.html'


class OrderableListView(views.OrderableListMixin, ListView):
    model = Article
    orderable_columns = ('id', 'title', )
    orderable_columns_default = 'id'


class CanonicalSlugDetailView(views.CanonicalSlugDetailMixin, DetailView):
    model = Article
    template_name = 'blank.html'


class OverriddenCanonicalSlugDetailView(views.CanonicalSlugDetailMixin,
                                        DetailView):
    model = Article
    template_name = 'blank.html'

    def get_canonical_slug(self):
        return codecs.encode(self.get_object().slug, 'rot_13')


class CanonicalSlugDetailCustomUrlKwargsView(views.CanonicalSlugDetailMixin,
                                             DetailView):
    model = Article
    template_name = 'blank.html'
    pk_url_kwarg = 'my_pk'
    slug_url_kwarg = 'my_slug'


class ModelCanonicalSlugDetailView(views.CanonicalSlugDetailMixin,
                                   DetailView):
    model = CanonicalArticle
    template_name = 'blank.html'


class FormMessagesView(views.FormMessagesMixin, CreateView):
    form_class = ArticleForm
    form_invalid_message = _('Invalid')
    form_valid_message = _('Valid')
    model = Article
    success_url = '/form_messages/'
    template_name = 'form.html'


class GroupRequiredView(views.GroupRequiredMixin, OkView):
    group_required = 'test_group'


class UserPassesTestView(views.UserPassesTestMixin, OkView):
    def test_func(self, user):
        return user.is_staff and not user.is_superuser \
            and user.email.endswith('@mydomain.com')


class UserPassesTestLoginRequiredView(views.LoginRequiredMixin,
                                      views.UserPassesTestMixin, OkView):
    def test_func(self, user):
        return user.is_staff and not user.is_superuser \
            and user.email.endswith('@mydomain.com')


class UserPassesTestNotImplementedView(views.UserPassesTestMixin, OkView):
    pass


class AllVerbsView(views.AllVerbsMixin, View):
    def all(self, request, *args, **kwargs):
        return HttpResponse('All verbs return this!')


class SSLRequiredView(views.SSLRequiredMixin, OkView):
    pass


class RecentLoginRequiredView(views.RecentLoginRequiredMixin, OkView):
    """
    A view for testing RecentLoginRequiredMixin.
    """
