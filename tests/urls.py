from django.core.exceptions import ImproperlyConfigured
from django.contrib.auth.views import LoginView
from django.urls import include, re_path
from . import views

urlpatterns = [
    # LoginRequiredMixin tests
    re_path(r"^login_required/$", views.LoginRequiredView.as_view()),
    # AnonymousRequiredView tests
    re_path(
        r"^unauthenticated_view/$",
        views.AnonymousRequiredView.as_view(),
        name="unauthenticated_view",
    ),
    re_path(
        r"^authenticated_view/$",
        views.AuthenticatedView.as_view(),
        name="authenticated_view",
    ),
    # AjaxResponseMixin tests
    re_path(r"^ajax_response/$", views.AjaxResponseView.as_view()),
    # CreateAndRedirectToEditView tests
    re_path(r"^article/create/$", views.CreateArticleView.as_view()),
    re_path(
        r"^article/(?P<pk>\d+)/edit/$",
        views.EditArticleView.as_view(),
        name="edit_article",
    ),
    re_path(
        r"^article_list/create/$",
        views.CreateArticleAndRedirectToListView.as_view(),
    ),
    re_path(
        r"^article_list_bad/create/$",
        views.CreateArticleAndRedirectToListViewBad.as_view(),
    ),
    re_path(
        r"^article_list/$",
        views.ArticleListView.as_view(),
        name="article_list",
    ),
    # CanonicalSlugDetailMixin tests
    re_path(
        r"^article-canonical/(?P<pk>\d+)-(?P<slug>[-\w]+)/$",
        views.CanonicalSlugDetailView.as_view(),
        name="canonical_slug",
    ),
    re_path(
        r"^article-canonical-override/(?P<pk>\d+)-(?P<slug>[-\w]+)/$",
        views.OverriddenCanonicalSlugDetailView.as_view(),
        name="canonical_override",
    ),
    re_path(
        r"^article-canonical-custom-kwargs/(?P<my_pk>\d+)-(?P<my_slug>[-\w]+)/$",
        views.CanonicalSlugDetailCustomUrlKwargsView.as_view(),
        name="canonical_custom_kwargs",
    ),
    re_path(
        r"^article-canonical-model/(?P<pk>\d+)-(?P<slug>[-\w]+)/$",
        views.ModelCanonicalSlugDetailView.as_view(),
        name="canonical_model",
    ),
    # UserFormKwargsMixin tests
    re_path(r"^form_with_user_kwarg/$", views.FormWithUserKwargView.as_view()),
    # SetHeadlineMixin tests
    re_path(r"^headline/$", views.HeadlineView.as_view(), name="headline"),
    re_path(r"^headline/lazy/$", views.LazyHeadlineView.as_view()),
    re_path(r"^headline/(?P<s>[\w-]+)/$", views.DynamicHeadlineView.as_view()),
    # ExtraContextMixin tests
    re_path(r"^context/$", views.ContextView.as_view(), name="context"),
    # PermissionRequiredMixin tests
    re_path(r"^permission_required/$", views.PermissionRequiredView.as_view()),
    # MultiplePermissionsRequiredMixin tests
    re_path(
        r"^multiple_permissions_required/$",
        views.MultiplePermissionsRequiredView.as_view(),
    ),
    # SuperuserRequiredMixin tests
    re_path(r"^superuser_required/$", views.SuperuserRequiredView.as_view()),
    # StaffuserRequiredMixin tests
    re_path(r"^staffuser_required/$", views.StaffuserRequiredView.as_view()),
    # GroupRequiredMixin tests
    re_path(r"^group_required/$", views.GroupRequiredView.as_view()),
    # UserPassesTestMixin tests
    re_path(r"^user_passes_test/$", views.UserPassesTestView.as_view()),
    # UserPassesTestMixin tests
    re_path(
        r"^user_passes_test_not_implemented/$",
        views.UserPassesTestNotImplementedView.as_view(),
    ),
    # CsrfExemptMixin tests
    re_path(r"^csrf_exempt/$", views.CsrfExemptView.as_view()),
    # JSONResponseMixin tests
    re_path(r"^simple_json/$", views.SimpleJsonView.as_view()),
    re_path(
        r"^simple_json_custom_encoder/$", views.CustomJsonEncoderView.as_view()
    ),
    re_path(r"^simple_json_400/$", views.SimpleJsonBadRequestView.as_view()),
    re_path(r"^article_list_json/$", views.ArticleListJsonView.as_view()),
    # JsonRequestResponseMixin tests
    re_path(r"^json_request/$", views.JsonRequestResponseView.as_view()),
    re_path(r"^json_bad_request/$", views.JsonBadRequestView.as_view()),
    re_path(
        r"^json_custom_bad_request/$", views.JsonCustomBadRequestView.as_view()
    ),
    # FormMessagesMixin tests
    re_path(r"form_messages/$", views.FormMessagesView.as_view()),
    # AllVerbsMixin tests
    re_path(r"all_verbs/$", views.AllVerbsView.as_view()),
    re_path(
        r"all_verbs_no_handler/$", views.AllVerbsView.as_view(all_handler=None)
    ),
    # SSLRequiredMixin tests
    re_path(r"^sslrequired/$", views.SSLRequiredView.as_view()),
    # RecentLoginRequiredMixin tests
    re_path(r"^recent_login/$", views.RecentLoginRequiredView.as_view()),
    re_path(r"^outdated_login/$", views.RecentLoginRequiredView.as_view()),
    # HeaderMixin tests
    re_path(r"^headers/attribute/$", views.AttributeHeaderView.as_view()),
    re_path(r"^headers/method/$", views.MethodHeaderView.as_view()),
    re_path(r"^headers/existing/$", views.ExistingHeaderView.as_view()),
]

urlpatterns += [
    re_path(
        r"^accounts/login/$", LoginView.as_view(template_name="blank.html")
    ),
    re_path(r"^auth/login/$", LoginView.as_view(template_name="blank.html")),
]

urlpatterns += [
    re_path(
        r"^article-canonical-namespaced/",
        include(
            ("tests.urls_namespaced", "tests"), namespace="some_namespace"
        ),
    ),
]
