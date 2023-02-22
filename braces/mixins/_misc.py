from django.core.exceptions import ImproperlyConfigured


class StaticContextMixin:
    static_context: dict = None

    def get_static_context(self) -> dict:
        class_name = self.__class__.__name__
        if self.static_context is None:
            raise ImproperlyConfigured(
                f"{class_name} is missing the static_context attribute. Define "
                f"{class_name}.static_context, or override {class_name}.get_static_context()"
            )
        if not isinstance(self.static_context, dict):
            raise ImproperlyConfigured(
                f"{class_name}.static_context must be a "
                "dictionary or a series of two-tuples."
            )
        return self.static_context

    def get_context_data(self, **kwargs) -> dict:
        context = super().get_context_data(**kwargs)
        context.update(self.get_static_context())
        return context


class SetHeadlineMixin(StaticContextMixin):
    headline: str = None

    def get_headline(self) -> str:
        class_name = self.__class__.__name__
        if self.headline is None:
            raise ImproperlyConfigured(
                f"{class_name} is missing the headline attribute. "
                f"Define {class_name}.headline, or override {class_name}.get_headline()."
            )
        return self.headline

    def get_static_context(self) -> dict:
        try:
            context = super().get_static_context()
        except ImproperlyConfigured:
            context = {}
        context.update({"headline": self.get_headline()})
        return context
