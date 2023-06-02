from __future__ import annotations
from typing import Protocol, Any
from .access import *
from .forms import *
from .http import *
from .json import *
from .messages import *
from .misc import *
from .queries import *
from .redirects import *
from .rest_framework import *

class HasContext(Protocol):
    """The concept of `context`."""

    context: dict[str, Any]

    def get_context_data(self) -> dict[str, Any]: ...
