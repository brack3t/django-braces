from django.urls import re_path

from . import views


urlpatterns = [
    re_path(
        r"^article/(?P<pk>\d+)-(?P<slug>[\w-]+)/$",
        views.CanonicalSlugDetailView.as_view(),
        name="namespaced_article",
    ),
]
