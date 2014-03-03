:orphan:

=========
Changelog
=========

Changes in version 1.4.0 (2014-03-05)
=====================================

* :issue:`105` Fixed bug in :ref:`GroupRequiredMixin` where superusers were blocked by lack of group memberships.
* :issue:`106` Fixed bug in :ref:`GroupRequiredMixin` which now correctly checks for group membership against a list.
* :feature:`102` Added new :ref:`StaticContextMixin` mixin which lets you pass in ``static_context`` as a property of the view.
* :feature:`89` Added new :ref:`AnonymousRequiredMixin` which redirects authenticated users to another view.
