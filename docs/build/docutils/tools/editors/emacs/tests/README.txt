====================
  Tests for rst.el
====================

:Author: Martin Blais <blais@furius.ca>, Stefan Merten <smerten@oekonux.de>

The tests are using ERT_. You need to install ERT_ for to run them. If
you did you should change the path to the library contained in the
variable `ERT` in `Makefile`.

To run the tests in Emacs use the facilities provided by ERT_. Namely
evaluate the buffer containing the tests and do::

  M-x ert [RETURN] [RETURN]

To run the tests by `make` use ::

  make ert-tests

.. _ERT: http://www.emacswiki.org/emacs/ErtTestLibrary
