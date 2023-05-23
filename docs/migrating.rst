Migrating from braces 1.x to 2.x
================================

``django-braces`` 2.x is a complete rewrite of the library. It is not backwards
compatible with 1.x. The main reason for this is that the 1.x version was
written when we were much more naive about Python and Django. Now, with
much more experience, we can provide you with more powerful, more flexible,
and simpler mixins.

If you install ``django-braces`` 2.x, **your code will break**.
**Your site will crash.** **Your dog will run away.** **Your cat will fall
in with a mouse gang.** No one wants these things to happen!

The migration path isn't pristine or smooth, but you'll have better views
at the end of it. Here's how to get there.

1. Install ``django-braces`` 2.x
2. Change all of your import from ``braces.views`` to ``braces.mixins``
3. Change mixin names, attributes, and method names as needed
4. Run tests to find which mixins are now missing
5. Recreate those mixins

We removed a lot of mixins that were redundant or which could be replaced
easily with a combination of mixins or a few lines of code. For example,
there was a `SuccessURLRedirectListMixin` which would redirect your user
to a list URL after a successful POST. You can replicate this with a
combination of :ref:``RedirectMixin`` and a bit of custom code to
trigger the redirect.

::

    from braces.mixins import RedirectMixin


    class SuccessURLRedirectListMixin(RedirectMixin):
        success_url = "/list"
        def form_valid(self, form):
            return self.redirect()

You'll find other mixins that require a similar level of replacement. If
you find that you're missing a mixin that you need, please open an issue
or, better yet, submit a pull request with the mixin.
