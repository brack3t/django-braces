from django.urls import include, re_path

from . import views

urlpatterns = [
    # CanonicalSlugDetailMixin namespace tests
    re_path(
        r"^article/(?P<pk>\d+)-(?P<slug>[\w-]+)/$",
        views.CanonicalSlugDetailView.as_view(),
        name="namespaced_article",
    ),
]
