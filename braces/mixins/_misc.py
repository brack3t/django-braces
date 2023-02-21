from django.core.exceptions import ImproperlyConfigured


class StaticContextMixin:
    """A mixin for adding static items to the context."""

    static_context: dict = None

    def get_static_context(self) -> dict:
        """Get the static context to add to the view's context."""
        class_name = self.__class__.__name__
        if self.static_context is None:
            raise ImproperlyConfigured(
                f"{class_name} is missing the static_context attribute. "
                f"Define {class_name}.static_context, or override "
                f"{class_name}.get_static_context()"
            )
        if not isinstance(self.static_context, dict):
            raise ImproperlyConfigured(
                f"{class_name}.static_context must be a "
                "dictionary or a series of two-tuples."
            )
        return self.static_context

    def get_context_data(self, **kwargs) -> dict:
        """Add the static context to the view's context."""
        context = super().get_context_data(**kwargs)
        context.update(self.get_static_context())
        return context
