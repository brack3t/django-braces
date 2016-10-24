from __future__ import absolute_import

from django.contrib.auth.views import login
from . import views
from .compat import include, url, patterns_compat

urlpatterns = [
    # LoginRequiredMixin tests
    url(r'^login_required/$', views.LoginRequiredView.as_view()),

    # AnonymousRequiredView tests
    url(r'^unauthenticated_view/$', views.AnonymousRequiredView.as_view(),
        name='unauthenticated_view'),
    url(r'^authenticated_view/$', views.AuthenticatedView.as_view(),
        name='authenticated_view'),

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
        views.CanonicalSlugDetailView.as_view(),
        name="canonical_slug"),
    url(r'^article-canonical-namespaced/',
        include('tests.urls_namespaced', namespace='some_namespace')),
    url(r'^article-canonical-override/(?P<pk>\d+)-(?P<slug>[-\w]+)/$',
        views.OverriddenCanonicalSlugDetailView.as_view(),
        name="canonical_override"),
    url(r'^article-canonical-custom-kwargs/(?P<my_pk>\d+)-(?P<my_slug>[-\w]+)/$',
        views.CanonicalSlugDetailCustomUrlKwargsView.as_view(),
        name="canonical_custom_kwargs"),
    url(r'^article-canonical-model/(?P<pk>\d+)-(?P<slug>[-\w]+)/$',
        views.ModelCanonicalSlugDetailView.as_view(),
        name="canonical_model"),

    # UserFormKwargsMixin tests
    url(r'^form_with_user_kwarg/$', views.FormWithUserKwargView.as_view()),

    # SetHeadlineMixin tests
    url(r'^headline/$', views.HeadlineView.as_view(), name='headline'),
    url(r'^headline/lazy/$', views.LazyHeadlineView.as_view()),
    url(r'^headline/(?P<s>[\w-]+)/$', views.DynamicHeadlineView.as_view()),

    # ExtraContextMixin tests
    url(r'^context/$', views.ContextView.as_view(), name='context'),

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
    url(r'^user_passes_test_not_implemented/$',
        views.UserPassesTestNotImplementedView.as_view()),

    # CsrfExemptMixin tests
    url(r'^csrf_exempt/$', views.CsrfExemptView.as_view()),

    # JSONResponseMixin tests
    url(r'^simple_json/$', views.SimpleJsonView.as_view()),
    url(r'^simple_json_custom_encoder/$',
        views.CustomJsonEncoderView.as_view()),
    url(r'^simple_json_400/$', views.SimpleJsonBadRequestView.as_view()),
    url(r'^article_list_json/$', views.ArticleListJsonView.as_view()),

    # JsonRequestResponseMixin tests
    url(r'^json_request/$', views.JsonRequestResponseView.as_view()),
    url(r'^json_bad_request/$', views.JsonBadRequestView.as_view()),
    url(r'^json_custom_bad_request/$',
        views.JsonCustomBadRequestView.as_view()),

    # FormMessagesMixin tests
    url(r'form_messages/$', views.FormMessagesView.as_view()),

    # AllVerbsMixin tests
    url(r'all_verbs/$', views.AllVerbsView.as_view()),
    url(r'all_verbs_no_handler/$',
        views.AllVerbsView.as_view(all_handler=None)),

    # SSLRequiredMixin tests
    url(r'^sslrequired/$', views.SSLRequiredView.as_view()),

    # RecentLoginRequiredMixin tests
    url(r'^recent_login/$', views.RecentLoginRequiredView.as_view()),
    url(r'^outdated_login/$', views.RecentLoginRequiredView.as_view()),
]

urlpatterns += [
    url(r'^accounts/login/$', login, {'template_name': 'blank.html'}),
    url(r'^auth/login/$', login, {'template_name': 'blank.html'}),
]

urlpatterns = patterns_compat(urlpatterns)
