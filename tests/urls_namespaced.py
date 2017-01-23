from __future__ import absolute_import

from . import views
from .compat import url, patterns_compat

urlpatterns = [
    # CanonicalSlugDetailMixin namespace tests
    url(r'^article/(?P<pk>\d+)-(?P<slug>[\w-]+)/$',
        views.CanonicalSlugDetailView.as_view(),
        name="namespaced_article"),
]

urlpatterns = patterns_compat(urlpatterns)
