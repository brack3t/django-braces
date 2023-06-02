from __future__ import annotations
from typing import *

from . import K, HasContext

class StaticContextMixin(HasContext):
    context: K
    static_context: K
    def get_static_context(self) -> K: ...
    def get_context_data(self) -> K: ...
