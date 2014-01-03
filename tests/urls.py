from . import views
from .compat import patterns, include, url


urlpatterns = patterns(
    '',
    # LoginRequiredMixin tests
    url(r'^login_required/$', views.LoginRequiredView.as_view()),

    # AjaxResponseMixin tests
    url(r'^ajax_response/$', views.AjaxResponseView.as_view()),

    # CreateAndRedirectToEditView tests
    url(r'^article/create/$', views.CreateArticleView.as_view()),
    url(r'^article/(?P<pk>\d+)/edit/$', views.EditArticleView.as_view(),
        name="edit_article"),

    url(r'^article_list/create/$',
        views.CreateArticleAndRedirectToListView.as_view()),
    url(r'^article_list_bad/create/$',
        views.CreateArticleAndRedirectToListViewBad.as_view()),
    url(r'^article_list/$', views.ArticleListView.as_view(),
        name='article_list'),

    # CanonicalSlugDetailMixin tests
    url(r'^article-canonical/(?P<pk>\d+)-(?P<slug>[-\w]+)/$',
        views.CanonicalSlugDetailView.as_view()),
    url(r'^article-canonical-namespaced/',
        include('tests.urls_namespaced', namespace='some_namespace')),
    url(r'^article-canonical-override/(?P<pk>\d+)-(?P<slug>[-\w]+)/$',
        views.OverriddenCanonicalSlugDetailView.as_view()),
    url(r'^article-canonical-model/(?P<pk>\d+)-(?P<slug>[-\w]+)/$',
        views.ModelCanonicalSlugDetailView.as_view()),

    # UserFormKwargsMixin tests
    url(r'^form_with_user_kwarg/$', views.FormWithUserKwargView.as_view()),

    # SetHeadlineMixin tests
    url(r'^headline/$', views.HeadlineView.as_view(), name='headline'),
    url(r'^headline/(?P<s>[\w-]+)/$', views.DynamicHeadlineView.as_view()),

    # PermissionRequiredMixin tests
    url(r'^permission_required/$', views.PermissionRequiredView.as_view()),

    # MultiplePermissionsRequiredMixin tests
    url(r'^multiple_permissions_required/$',
        views.MultiplePermissionsRequiredView.as_view()),

    # SuperuserRequiredMixin tests
    url(r'^superuser_required/$', views.SuperuserRequiredView.as_view()),

    # StaffuserRequiredMixin tests
    url(r'^staffuser_required/$', views.StaffuserRequiredView.as_view()),

    # GroupRequiredMixin tests
    url(r'^group_required/$', views.GroupRequiredView.as_view()),

    # UserPassesTestMixin tests
    url(r'^user_passes_test/$', views.UserPassesTestView.as_view()),

    # UserPassesTestMixin tests
    url(r'^user_passes_test_not_implemented/$', views.UserPassesTestNotImplementedView.as_view()),

    # CsrfExemptMixin tests
    url(r'^csrf_exempt/$', views.CsrfExemptView.as_view()),

    # JSONResponseMixin tests
    url(r'^simple_json/$', views.SimpleJsonView.as_view()),
    url(r'^simple_json_400/$', views.SimpleJsonBadRequestView.as_view()),
    url(r'^article_list_json/$', views.ArticleListJsonView.as_view()),

    # JsonRequestResponseMixin tests
    url(r'^json_request/$', views.JsonRequestResponseView.as_view()),
    url(r'^json_bad_request/$', views.JsonBadRequestView.as_view()),
    url(r'^json_custom_bad_request/$', views.JsonCustomBadRequestView.as_view()),

    # FormMessagesMixin tests
    url(r'form_messages/$', views.FormMessagesView.as_view()),
)


urlpatterns += patterns(
    'django.contrib.auth.views',
    # login page, required by some tests
    url(r'^accounts/login/$', 'login', {'template_name': 'blank.html'}),
    url(r'^auth/login/$', 'login', {'template_name': 'blank.html'}),
)
