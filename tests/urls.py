from django.contrib.auth.views import LoginView
from django.urls import path
from django.urls import include, re_path
from . import views

urlpatterns = [
    # LoginRequiredMixin tests
    path("login_required/", views.LoginRequiredView.as_view()),
    # AnonymousRequiredView tests
    path(
        "unauthenticated_view/",
        views.AnonymousRequiredView.as_view(),
        name="unauthenticated_view",
    ),
    path(
        "authenticated_view/",
        views.AuthenticatedView.as_view(),
        name="authenticated_view",
    ),
    # AjaxResponseMixin tests
    path("ajax_response/", views.AjaxResponseView.as_view()),
    # CreateAndRedirectToEditView tests
    path("article/create/", views.CreateArticleView.as_view()),
    path(
        "article/<int:pk>/edit/",
        views.EditArticleView.as_view(),
        name="edit_article",
    ),
    path(
        "article_list/create/",
        views.CreateArticleAndRedirectToListView.as_view(),
    ),
    path(
        "article_list_bad/create/",
        views.CreateArticleAndRedirectToListViewBad.as_view(),
    ),
    path(
        "article_list/",
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
    path("form_with_user_kwarg/", views.FormWithUserKwargView.as_view()),
    # SetHeadlineMixin tests
    path("headline/", views.HeadlineView.as_view(), name="headline"),
    path("headline/lazy/", views.LazyHeadlineView.as_view()),
    re_path(r"^headline/(?P<s>[\w-]+)/$", views.DynamicHeadlineView.as_view()),
    # ExtraContextMixin tests
    path("context/", views.ContextView.as_view(), name="context"),
    # PermissionRequiredMixin tests
    path("permission_required/", views.PermissionRequiredView.as_view()),
    path("object_level_permission_required/", views.PermissionRequiredView.as_view(object_level_permissions=True)),
    # MultiplePermissionsRequiredMixin tests
    path(
        "multiple_permissions_required/",
        views.MultiplePermissionsRequiredView.as_view(),
    ),
    path(
        "multiple_object_level_permissions_required/",
        views.MultiplePermissionsRequiredView.as_view(object_level_permissions=True),
    ),
    # SuperuserRequiredMixin tests
    path("superuser_required/", views.SuperuserRequiredView.as_view()),
    # StaffuserRequiredMixin tests
    path("staffuser_required/", views.StaffuserRequiredView.as_view()),
    # GroupRequiredMixin tests
    path("group_required/", views.GroupRequiredView.as_view()),
    # UserPassesTestMixin tests
    path("user_passes_test/", views.UserPassesTestView.as_view()),
    # UserPassesTestMixin tests
    path(
        "user_passes_test_not_implemented/",
        views.UserPassesTestNotImplementedView.as_view(),
    ),
    # CsrfExemptMixin tests
    path("csrf_exempt/", views.CsrfExemptView.as_view()),
    # JSONResponseMixin tests
    path("simple_json/", views.SimpleJsonView.as_view()),
    path(
        "simple_json_custom_encoder/", views.CustomJsonEncoderView.as_view()
    ),
    path("simple_json_400/", views.SimpleJsonBadRequestView.as_view()),
    path("article_list_json/", views.ArticleListJsonView.as_view()),
    # JsonRequestResponseMixin tests
    path("json_request/", views.JsonRequestResponseView.as_view()),
    path("json_bad_request/", views.JsonBadRequestView.as_view()),
    path(
        "json_custom_bad_request/", views.JsonCustomBadRequestView.as_view()
    ),
    # FormMessagesMixin tests
    path("form_messages/", views.FormMessagesView.as_view()),
    # AllVerbsMixin tests
    path("all_verbs/", views.AllVerbsView.as_view()),
    path(
        "all_verbs_no_handler/", views.AllVerbsView.as_view(all_handler=None)
    ),
    # SSLRequiredMixin tests
    path("sslrequired/", views.SSLRequiredView.as_view()),
    # RecentLoginRequiredMixin tests
    path("recent_login/", views.RecentLoginRequiredView.as_view()),
    path("outdated_login/", views.RecentLoginRequiredView.as_view()),
    # HeaderMixin tests
    path('headers/attribute/', views.AttributeHeaderView.as_view()),
    path('headers/method/', views.MethodHeaderView.as_view()),
    path('headers/existing/', views.ExistingHeaderView.as_view()),
    # CacheControlMixin tests
    path('cachecontrol/public/', views.CacheControlPublicView.as_view()),
    # NeverCacheMixin tests
    path('nevercache/', views.NeverCacheView.as_view()),
]

urlpatterns += [
    path(
        "accounts/login/", LoginView.as_view(template_name="blank.html")
    ),
    path("auth/login/", LoginView.as_view(template_name="blank.html")),
]

urlpatterns += [
    path(
        "article-canonical-namespaced/",
        include(
            ("tests.urls_namespaced", "tests"), namespace="some_namespace"
        ),
    ),
]
