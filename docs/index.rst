.. django-braces documentation master file, created by
   sphinx-quickstart on Mon Apr 30 10:31:44 2012.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to django-braces's documentation!
=========================================

``django-braces`` is a set of mixins to extend the functionality of Django's `generic class-based views`_. Most of our mixins are aimed at permissions, queryset manipulation, and view functionality (sending back ``JSON`` or creating success/error messages using the messages framework).


Contributing
------------

If you'd like to contribute (thanks!), please keep the following in mind:

* We're not interested in copying the functionality of other mixin-centric projects like `django-model-utils`_ or `django-extra-views`_.
* All pull requests need to have accompanying tests. If you fix a bug, please write a regression test.
* All pull requests also need to have documentation with them. Please provide a rationale for the mixin and at least one code example of using it in a view.
* Before you spend your time writing a large mixin with tests and docs, feel free to ask us if we think it would be a good addition to the project as a Github issue.

You can view the code of our project on `Github`_.

Contents
--------

.. toctree::
    :maxdepth: 2

    Access Mixins <access>
    Form Mixins <form>
    Other Mixins <other>



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`


.. _Github: https://github.com/brack3t/django-braces
.. _generic class-based views: https://docs.djangoproject.com/en/1.5/topics/class-based-views/
.. _django-model-utils: https://github.com/carljm/django-model-utils
.. _django-extra-views: https://github.com/AndrewIngram/django-extra-views
