from __future__ import absolute_import

from tests.compat import url, patterns_compat
from tests.testapp import views

urlpatterns = [
    # CanonicalSlugDetailMixin namespace tests
    url(r'^article/(?P<pk>\d+)-(?P<slug>[\w-]+)/$',
        views.CanonicalSlugDetailView.as_view(),
        name="namespaced_article"),
]

urlpatterns = patterns_compat(urlpatterns)
