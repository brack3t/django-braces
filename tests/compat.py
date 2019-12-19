try:
    from django.utils.encoding import force_str as force_string
except ImportError:
    from django.utils.encoding import force_text as force_string

try:
    from django.conf.urls import url, include
except ImportError:
    from django.conf.urls.defaults import url, include


def patterns_compat(urlpatterns):
    try:
        from django.conf.urls import patterns
    except ImportError:
        try:
            from django.conf.urls.defaults import patterns
        except ImportError:
            patterns = False
    if patterns:
        return patterns(
            '', *urlpatterns
        )
    else:
        return urlpatterns
