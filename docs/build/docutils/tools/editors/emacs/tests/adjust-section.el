;; Tests for rst-adjust

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-adjust ()
  "Tests for `rst-adjust'."
  (let ( ;; Set customizable variables to defined values
	(rst-new-adornment-down t)
	(rst-default-indent 1)
	(rst-preferred-adornments '((?= over-and-under 1)
				    (?= simple 0)
				    (?- simple 0)
				    (?~ simple 0)
				    (?+ simple 0)
				    (?` simple 0)
				    (?# simple 0)
				    (?@ simple 0))))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Some Title\^@

"
	     "
============
 Some Title
============

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Some Title
\^@
"
	     "
============
 Some Title
============

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Some Tit\^@le

"
	     "
============
 Some Title
============

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
\^@Some Title

"
	     "
============
 Some Title
============

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Some Title\^@

Other Title
-----------

Other Title2
~~~~~~~~~~~~

"
	     "
============
 Some Title
============

Other Title
-----------

Other Title2
~~~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
Some Title\^@

"
	     "
Some Title
==========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
   Some Title\^@

"
	     "
================
   Some Title
================

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
   Some Title\^@

"
	     "
Some Title
==========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
--------------

Some Title\^@

"
	     "
Previous Title
--------------

Some Title
~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
--------------

Some Title\^@

Next Title
~~~~~~~~~~

"
	     "
Previous Title
--------------

Some Title
~~~~~~~~~~

Next Title
~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
Previous Title
--------------

Some Title\^@

"
	     "
Previous Title
--------------

~~~~~~~~~~
Some Title
~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
Previous Title
--------------

  Some Title\^@

"
	     "
Previous Title
--------------

~~~~~~~~~~~~~~
  Some Title
~~~~~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
--------------

   Some Title\^@

"
	     "
Previous Title
--------------

Some Title
~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust -)
	     "
Previous Title
--------------

Some Title\^@

Next Title
~~~~~~~~~~
"
	     "
Previous Title
--------------

Some Title
----------

Next Title
~~~~~~~~~~
"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title\^@
----------
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
----------\^@
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
-\^@
"
	     "
Previous Title
-
"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
--\^@
"
	     "
Previous Title
--
"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
---\^@
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Previous Title
------------------\^@
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
----------------
 Previous Title
----------\^@
"
	     "
----------------
 Previous Title
----------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
----------\^@
 Previous Title
----------------
"
	     "
----------------
 Previous Title
----------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
----------
 Previous Title\^@
-----
"
	     "
----------------
 Previous Title
----------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
Previous Title
----------\^@
"
	     "
--------------
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
----------------
 Previous Title\^@
--------
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
--------\^@
 Previous Title
----------------
"
	     "
Previous Title
--------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "--------\^@
 Previous Title
----------------
"
	     "----------------
 Previous Title
----------------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "=======
Document Title\^@
==============
"
	     "==============
Document Title
==============

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
================
 Document Title
================

SubTitle
--------

My Title\^@
--------

After Title
~~~~~~~~~~~

"
	     "
================
 Document Title
================

SubTitle
--------

==========
 My Title
==========

After Title
~~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust -)
	     "
================
 Document Title
================

SubTitle
--------

My Title\^@
--------

After Title
~~~~~~~~~~~

"
	     "
================
 Document Title
================

SubTitle
--------

My Title
~~~~~~~~

After Title
~~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust -)
	     "
================
 Document Title
================

SubTitle
========

My Title\^@
========

"
	     "
================
 Document Title
================

SubTitle
========

My Title
--------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust -)
	     "
================
 Document Title
================

SubTitle
========

My Title\^@
--------

"
	     "
================
 Document Title
================

SubTitle
========

==========
 My Title
==========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
================
 Document Title
================

SubTitle
========

==========
 My Title\^@
==========

"
	     "
================
 Document Title
================

SubTitle
========

My Title
--------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
================
 Document Title
================

SubTitle
========

My Title\^@
--------
"
	     "
================
 Document Title
================

SubTitle
========

My Title
========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
SubTitle\^@
~~~~~~~~

"
	     "
~~~~~~~~~~
 SubTitle
~~~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust 1)
	     "
~~~~~~~~~~
 SubTitle\^@
~~~~~~~~~~

"
	     "
SubTitle
~~~~~~~~

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Document Title\^@

"
	     "
================
 Document Title\^@
================

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "

Document Title\^@
"
	     "

================
 Document Title\^@
================

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "

Document Title\^@"
	     "

================
 Document Title\^@
================
"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
Document Title
==============
Subtitle\^@

"
	     "
Document Title
==============
Subtitle\^@
--------

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "==============
Document Title\^@
==============
Subtitle
========

"
	     "Document Title\^@
==============
Subtitle
========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
==============
Document Title\^@
==============
Subtitle
========

"
	     "
Document Title\^@
==============
Subtitle
========

"
	     t))
    (should (equal-buffer
	     '(rst-adjust)
	     "
==============
Document Title
==============
===============
Document Title2\^@
===============

"
	     "
==============
Document Title
==============
Document Title2
===============

"
	     t))
    ;; docutils-Bugs #2972588
    (should (equal-buffer
	     '(rst-adjust)
	     "
==============
Document Title
==============

Subtitle
========

.. contents::
   :depth: 2
..
  1 Section 1
  2 Section 2

Section 1\^@
---------

Section 2
---------
"
	     "
==============
Document Title
==============

Subtitle
========

.. contents::
   :depth: 2
..
  1 Section 1
  2 Section 2

Section 1\^@
=========

Section 2
---------
"
	     t))

;; FIXME: todo
;; ;;------------------------------------------------------------------------------
;; (cycle-previous-only
;; "
;; ==================
;;   Document Title
;; ==================
;; 
;; Document Title2
;; ===============
;; 
;; =======
;;   Bli\^@
;; =======
;; 
;; Document Title2
;; ===============
;; 
;; Document Title3
;; ---------------
;; 
;; Document Title4
;; ~~~~~~~~~~~~~~~
;; 
;; "
;; "
;; ==================
;;   Document Title
;; ==================
;; 
;; Document Title2
;; ===============
;; 
;; Bli\^@
;; ---
;; 
;; Document Title2
;; ===============
;; 
;; Document Title3
;; ---------------
;; 
;; Document Title4
;; ~~~~~~~~~~~~~~~
;; 
;; "
;; )
    ))
