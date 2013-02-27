from django.http import HttpResponse
from django.views.generic import View, UpdateView, FormView, TemplateView
from django.views.generic import ListView
from braces.views import *
from .models import Article
from .forms import FormWithUserKwarg


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


class LoginRequiredView(LoginRequiredMixin, OkView):
    """
    A view for testing LoginRequiredMixin.
    """


class AjaxResponseView(AjaxResponseMixin, OkView):
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


class SimpleJsonView(JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_response() method.
    """
    def get(self, request):
        object = {'username': request.user.username}
        return self.render_json_response(object)


class ArticleListJsonView(JSONResponseMixin, View):
    """
    A view for testing JSONResponseMixin's render_json_object_response()
    method.
    """
    def get(self, request):
        queryset = Article.objects.all()
        return self.render_json_object_response(
            queryset, fields=('title',))


class CreateArticleView(CreateAndRedirectToEditView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    model = Article
    success_url_name = 'edit_article'
    template_name = 'form.html'


class EditArticleView(UpdateView):
    """
    View for testing CreateAndRedirectEditToView.
    """
    model = Article
    template_name = 'form.html'


class CreateArticleAndRedirectToListView(
        SuccessURLRedirectListMixin, CreateArticleView):
    """
    View for testing SuccessURLRedirectListMixin
    """
    success_list_url = 'article_list'


class CreateArticleAndRedirectToListViewBad(
        SuccessURLRedirectListMixin, CreateArticleView):
    """
    View for testing SuccessURLRedirectListMixin
    """
    success_list_url = None


class ArticleListView(SelectRelatedMixin, ListView):
    """
    A list view for articles, required for testing SuccessURLRedirectListMixin.

    Also used to test SelectRelatedMixin.
    """
    model = Article
    template_name = 'blank.html'
    select_related = ('author',)


class FormWithUserKwargView(UserFormKwargsMixin, FormView):
    """
    View for testing UserFormKwargsMixin.
    """
    form_class = FormWithUserKwarg
    template_name = 'form.html'

    def form_valid(self, form):
        return HttpResponse("username: %s" % form.user.username)


class HeadlineView(SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin.
    """
    template_name = 'blank.html'
    headline = "Test headline"


class DynamicHeadlineView(SetHeadlineMixin, TemplateView):
    """
    View for testing SetHeadlineMixin's get_headline() method.
    """
    template_name = 'blank.html'

    def get_headline(self):
        return self.kwargs['s']


class PermissionRequiredView(PermissionRequiredMixin, OkView):
    """
    View for testing PermissionRequiredMixin.
    """
    permission_required = 'auth.add_user'


class MultiplePermissionsRequiredView(
        MultiplePermissionsRequiredMixin, OkView):
    permissions = {
        'all': ['tests.add_article', 'tests.change_article'],
        'any': ['auth.add_user', 'auth.change_user'],
    }


class SuperuserRequiredView(SuperuserRequiredMixin, OkView):
    pass


class StaffuserRequiredView(StaffuserRequiredMixin, OkView):
    pass


class CsrfExemptView(CsrfExemptMixin, OkView):
    pass
