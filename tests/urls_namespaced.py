from . import views
from .compat import patterns, url


urlpatterns = patterns(
    '',
    # CanonicalSlugDetailMixin namespace tests
    url(r'^article/(?P<pk>\d+)-(?P<slug>[\w-]+)/$',
        views.CanonicalSlugDetailView.as_view(),
        name="namespaced_article"),
)
