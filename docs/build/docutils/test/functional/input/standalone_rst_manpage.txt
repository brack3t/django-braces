=========
 rst2man
=========

---------------------------------------------
generate unix manpages from reStructured text
---------------------------------------------

:Author: grubert@users.sourceforge.net
:Date:   2006-10-22
:Copyright: public domain
:Version: 0.1
:Manual section: 1
:Manual group: text processing

.. TODO: authors and author with name <email>

SYNOPSIS
========

  rst2man.py inputfile outputfile

DESCRIPTION
===========

rst2man transforms a reStructured text document into a unix man page.

In theory any valid reStructured text document should be processable,
in reality this is

* a goal, that is not met yet
* a goal that might never be met, because only few constructs are
  used in man pages *and* because the common text file does not adhere
  to man page requirements. 
  
  For example a unix man page belongs into a numbered section, 1 is 
  user commands, 8 contains administrator commands and the headlines
  of all manpages are collected into a database, queryable with the
  programm ``apropos``, therefore the headline should contain a short
  text describing into which group this command belongs.

  These informations are collected from title, subtitle and the
  docinfo, see this document as an example.

OPTIONS
=======

--config=<file>         Read configuration settings from <file>, if it exists.
--version, -V           Show this program's version number and exit.
--help, -h              Show this help message and exit.

And a lot more standard docutils options.

FILES
=====

None yet.

SEE ALSO
========

`docutils <http://docutils.sourceforge.net>`__
`linux man page howto <http://tldp.org/HOWTO/Man-Page/>`__

and ``man man`` also ``man 7 man``

BUGS
====

1. Format options are included as they are required. 
2. bullet lists
3. number lists
4. math: The LaTeX source is shown, e.g. :math:`n! + \sin(x_n^2)`.


Discussion is still open.


