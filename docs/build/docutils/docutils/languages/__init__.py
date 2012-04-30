# $Id: __init__.py 6433 2010-09-28 08:21:25Z milde $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

# Internationalization details are documented in
# <http://docutils.sf.net/docs/howto/i18n.html>.

"""
This package contains modules for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

from docutils.utils import normalize_language_tag

_languages = {}

def get_language(language_code, reporter):
    """Return module with language localizations.

    `language_code` is a "BCP 47" language tag.
    If there is no matching module, warn and fall back to English.
    """
    # TODO: use a dummy module returning emtpy strings?, configurable?
    for tag in normalize_language_tag(language_code):
        if tag in _languages:
            return _languages[tag]
        try:
            module = __import__(tag, globals(), locals())
        except ImportError:
            continue
        _languages[tag] = module
        return module
    reporter.warning(
        'language "%s" not supported: ' % language_code +
        'Docutils-generated text will be in English.')
    module = __import__('en', globals(), locals())
    _languages[tag] = module # warn only one time!
    return module
