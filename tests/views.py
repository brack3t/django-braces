import codecs

from django.contrib.auth.models import User
from django.http import HttpResponse
from django.views.generic import (View, UpdateView, FormView, TemplateView,
                                  ListView, DetailView, CreateView)

from braces import views

from .models import Article, CanonicalArticle
from .forms import ArticleForm, FormWithUserKwarg


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


class AjaxResponseView(views.AjaxResponseMixin, OkView):
    """
    A view for testing AjaxResponseMixin.
    """
    def get_ajax(self, request):
        return HttpResponse(u"AJAX_OK")

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
        object = {u'username': request.user.username}
        return self.render_json_response(object)


class SimpleJsonBadRequestView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_response() method with
    400 HTTP status code.
    """
    def get(self, request):
        object = {u'username': request.user.username}
        return self.render_json_response(object, status=400)


class ArticleListJsonView(views.JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_object_response()
    method.
    """
    def get(self, request):
        queryset = Article.objects.all()
        return self.render_json_object_response(
            queryset, fields=(u'title',))


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
                {u'error': 'you messed up'})
        return self.render_json_response(self.request_json)


class CreateArticleView(CreateView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    model = Article
    template_name = u'form.html'


class EditArticleView(UpdateView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    model = Article
    template_name = u'form.html'


class CreateArticleAndRedirectToListView(views.SuccessURLRedirectListMixin,
                                         CreateArticleView):
    """
    View for testing SuccessURLRedirectListMixin
    """
    success_list_url = u'article_list'


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
    template_name = u'blank.html'
    select_related = (u'author',)


class FormWithUserKwargView(views.UserFormKwargsMixin, FormView):
    """
    View for testing UserFormKwargsMixin.
    """
    form_class = FormWithUserKwarg
    template_name = u'form.html'

    def form_valid(self, form):
        return HttpResponse(u"username: {0.user.username}".format(form))


class HeadlineView(views.SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin.
    """
    template_name = u'blank.html'
    headline = u"Test headline"


class DynamicHeadlineView(views.SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin's get_headline() method.
    """
    template_name = u'blank.html'

    def get_headline(self):
        return self.kwargs[u's']


class PermissionRequiredView(views.PermissionRequiredMixin, OkView):
    """
    View for testing PermissionRequiredMixin.
    """
    permission_required = u'auth.add_user'


class MultiplePermissionsRequiredView(views.MultiplePermissionsRequiredMixin,
                                      OkView):
    permissions = {
        u'all': [u'tests.add_article', u'tests.change_article'],
        u'any': [u'auth.add_user', u'auth.change_user'],
    }


class SuperuserRequiredView(views.SuperuserRequiredMixin, OkView):
    pass


class StaffuserRequiredView(views.StaffuserRequiredMixin, OkView):
    pass


class CsrfExemptView(views.CsrfExemptMixin, OkView):
    pass


class AuthorDetailView(views.PrefetchRelatedMixin, ListView):
    model = User
    prefetch_related = [u'article_set']
    template_name = u'blank.html'


class OrderableListView(views.OrderableListMixin, ListView):
    model = Article
    orderable_columns = (u'id', u'title', )
    orderable_columns_default = u'id'


class CanonicalSlugDetailView(views.CanonicalSlugDetailMixin, DetailView):
    model = Article
    template_name = u'blank.html'


class OverriddenCanonicalSlugDetailView(views.CanonicalSlugDetailMixin,
                                        DetailView):
    model = Article
    template_name = u'blank.html'

    def get_canonical_slug(self):
        return codecs.encode(self.get_object().slug, u'rot_13')


class ModelCanonicalSlugDetailView(views.CanonicalSlugDetailMixin,
                                            DetailView):
    model = CanonicalArticle
    template_name = u'blank.html'


class FormMessagesView(views.FormMessagesMixin, CreateView):
    form_class = ArticleForm
    form_invalid_message = u'Invalid'
    form_valid_message = u'Valid'
    model = Article
    success_url = u'/form_messages/'
    template_name = u'form.html'


class GroupRequiredView(views.GroupRequiredMixin, OkView):
    group_required = u'test_group'
