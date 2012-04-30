;; Tests for various functions handling adornments

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun find-title-line ()
  "Wrapper for calling `rst-find-title-line'."
  (apply-adornment-match (rst-find-title-line)))

(ert-deftest rst-find-title-line ()
  "Tests for `rst-find-title-line'."
  (should (equal-buffer-return
	   '(find-title-line)
	   "

Du bon vin tous les jours.
\^@
"
	   "

\^@Du bon vin tous les jours.

"
	   '((nil . nil) nil "Du bon vin tous les jours." nil)
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
\^@
Du bon vin tous les jours.

"
	   "

\^@Du bon vin tous les jours.

"
	   '((nil . nil) nil "Du bon vin tous les jours." nil)
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "

Du bon vin tous les jours.
------\^@-----
"
	   "

\^@Du bon vin tous les jours.
-----------
"
	   '((?- . simple) nil "Du bon vin tous les jours." "-----------")
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
------\^@-----
Du bon vin tous les jours.

"
	   "
-----------
\^@Du bon vin tous les jours.

"
	   '((?- . nil) "-----------" "Du bon vin tous les jours." nil)
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
\^@-----------
Du bon vin tous les jours.
-----------

"
	   "
-----------
\^@Du bon vin tous les jours.
-----------

"
	   '((?- . over-and-under) "-----------" "Du bon vin tous les jours."
	     "-----------")
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
Du bon vin tous les jours.
\^@-----------
Du bon vin tous les jours.
-----------

"
	   "
Du bon vin tous les jours.
-----------
\^@Du bon vin tous les jours.
-----------

" ; This is not how the parser works but looks more logical
	   '((?- . over-and-under) "-----------" "Du bon vin tous les jours."
	     "-----------")
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "

\^@-----------

"
	   "

\^@-----------

"
	   nil
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
Line 1
\^@
Line 2

"
	   "
\^@Line 1

Line 2

"
	   '((nil . nil) nil "Line 1" nil)
	   ))
  (should (equal-buffer-return
	   '(find-title-line)
	   "
=====================================
   Project Idea: Panorama Stitcher
====================================

:Author: Martin Blais <blais@furius.ca>
\^@
Another Title
=============
"
	   "
=====================================
   Project Idea: Panorama Stitcher
====================================

\^@:Author: Martin Blais <blais@furius.ca>

Another Title
=============
"
	   '((nil . nil) nil ":Author: Martin Blais <blais@furius.ca>" nil)
	   ))
  )

(setq text-1
"===============================
   Project Idea: My Document
===============================

:Author: Martin Blais

Introduction
============

This is the introduction.

Notes
-----

Some notes.

Main Points
===========

Yep.

Super Point
-----------

~~~~~~~~~~~
\^@ Sub Point
~~~~~~~~~~~

Isn't this fabulous?

Conclusion
==========

That's it, really.

")

(setq text-2
"

Previous
--------

Current\^@
~~~~~~~

Next
++++

")

(setq text-3
"

Previous
--------

Current\^@
~~~~~~~

  Next
  ++++

")

(ert-deftest rst-find-all-adornments ()
  "Tests for `rst-find-all-adornments'."
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-1
	   nil
	   '((2 ?= over-and-under 3)
	     (7 ?= simple 0)
	     (12 ?- simple 0)
	     (17 ?= simple 0)
	     (22 ?- simple 0)
	     (26 ?~ over-and-under 1)
	     (31 ?= simple 0))
	   ))
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-2
	   nil
	   '((3 ?- simple 0)
	     (6 ?~ simple 0)
	     (9 ?+ simple 0))
	   ))
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-3
	   nil
	   '((3 ?- simple 0)
	     (6 ?~ simple 0))
	   ))
  )

(ert-deftest rst-get-hierarchy ()
  "Tests for `rst-get-hierarchy'."
  (should (equal-buffer-return
	   '(rst-get-hierarchy)
	   text-1
	   nil
	   '((?= over-and-under 3)
	     (?= simple 0)
	     (?- simple 0)
	     (?~ over-and-under 1))
	   ))
  )

(ert-deftest rst-get-hierarchy-ignore ()
  "Tests for `rst-get-hierarchy' with ignoring a line."
  (should (equal-buffer-return
	   '(rst-get-hierarchy 26)
	   text-1
	   nil
	   '((?= over-and-under 3)
	     (?= simple 0)
	     (?- simple 0))
	   ))
  )

(ert-deftest rst-adornment-level ()
  "Tests for `rst-adornment-level'."
  (should (equal-buffer-return
	   '(rst-adornment-level t)
	   text-1
	   nil
	   t
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level nil)
	   text-1
	   nil
	   nil
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level (?= . over-and-under))
	   text-1
	   nil
	   1
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level (?= . simple))
	   text-1
	   nil
	   2
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level (?- . simple))
	   text-1
	   nil
	   3
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level (?~ . over-and-under))
	   text-1
	   nil
	   4
	   ))
  (should (equal-buffer-return
	   '(rst-adornment-level (?# . simple))
	   text-1
	   nil
	   5
	   ))
  )

(ert-deftest rst-adornment-complete-p ()
  "Tests for `rst-adornment-complete-p'."
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "

\^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
======
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
======
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 2))
	   "
==========
\^@  Vaudou
==========
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
=====
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
=======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
===-==
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
=====
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
=====
\^@Vaudou
======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
===-==
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
===-==
\^@Vaudou
======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
------
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
=========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
=========
  \^@Vaudou
==========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
===-======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
===-======
  \^@Vaudou
==========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
----------
"
	   nil
	   nil))
  )

(ert-deftest rst-get-adornments-around ()
  "Tests for `rst-get-adornments-around'."
  (should (equal-buffer-return
	   '(rst-get-adornments-around)
	   "

Previous
--------

\^@Current

Next
++++

"
	   nil
	   '((?- simple 0) (?+ simple 0))))
  (should (equal-buffer-return
	   '(rst-get-adornments-around)
	   "

Previous
--------

Current\^@
~~~~~~~

Next
++++

"
	   nil
	   '((?- simple 0) (?+ simple 0))))
  )

(defun apply-adornment-match (match)
  "Apply the MATCH to the buffer and return important data.
MATCH is as returned by `rst-classify-adornment' or
`rst-find-title-line'. Puts point in the beginning of the title
line. Return a list consisting of (CHARACTER . STYLE) and the
three embedded match groups. Return nil if MATCH is nil. Checks
whether embedded match groups match match group 0."
  (when match
    (set-match-data (cdr match))
    (let ((whole (match-string-no-properties 0))
	  (over (match-string-no-properties 1))
	  (text (match-string-no-properties 2))
	  (under (match-string-no-properties 3))
	  (gather ""))
      (if over
	  (setq gather (concat gather over "\n")))
      (if text
	  (setq gather (concat gather text "\n")))
      (if under
	  (setq gather (concat gather under "\n")))
      (if (not (string= (substring gather 0 -1) whole))
	  (error "Match 0 '%s' doesn't match concatenated parts '%s'"
		 whole gather))
      (goto-char (match-beginning 2))
      (list (car match) over text under))))

(defun classify-adornment (beg end)
  "Wrapper for calling `rst-classify-adornment'."
  (interactive "r")
  (apply-adornment-match (rst-classify-adornment
			  (buffer-substring-no-properties beg end) end)))

(ert-deftest rst-classify-adornment ()
  "Tests for `rst-classify-adornment'."
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

Du bon vin tous les jours
\^@=========================\^?

"
	   nil
	   '((?= . simple)
	     nil "Du bon vin tous les jours" "=========================")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

Du bon vin tous les jours
\^@====================\^?

"
	   nil
	   '((?= . simple)
	     nil "Du bon vin tous les jours" "====================")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

     Du bon vin tous les jours
\^@====================\^?

"
	   nil
	   '((?= . simple)
	     nil "     Du bon vin tous les jours" "====================")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

Du bon vin tous les jours
\^@-\^?
"
	   nil
	   nil
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

Du bon vin tous les jours
\^@--\^?
"
	   nil
	   nil
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "

Du bon vin tous les jours
\^@---\^?
"
	   nil
	   '((?- . simple)
	     nil "Du bon vin tous les jours" "---")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
\^@~~~~~~~~~~~~~~~~~~~~~~~~~\^?
Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '((?~ . over-and-under)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~" "Du bon vin tous les jours" "~~~~~~~~~~~~~~~~~~~~~~~~~")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "~~~~~~~~~~~~~~~~~~~~~~~~~
Du bon vin tous les jours
\^@~~~~~~~~~~~~~~~~~~~~~~~~~\^?

"
	   nil
	   '((?~ . over-and-under)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~" "Du bon vin tous les jours" "~~~~~~~~~~~~~~~~~~~~~~~~~")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
\^@~~~~~~~~~~~~~~~~~~~~~~~~~\^?
   Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '((?~ . over-and-under)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~" "   Du bon vin tous les jours" "~~~~~~~~~~~~~~~~~~~~~~~~~")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
\^@~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\^?
Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '((?~ . over-and-under)
	     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" "Du bon vin tous les jours" "~~~~~~~~~~~~~~~~~~~")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
---------------------------
Du bon vin tous les jours
\^@~~~~~~~~~~~~~~~~~~~~~~~~~~~\^?

"
	   nil
	   '((?~ . simple)
	     nil "Du bon vin tous les jours" "~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "\^@---------------------------\^?"
	   nil
	   '(t
	     nil "---------------------------" nil)
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
\^@---------------------------\^?
Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   nil
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
=========================
Du bon vin tous les jours
\^@=========================\^?
Du bon vin

"
	   nil
	   '((?= . over-and-under)
	     "=========================" "Du bon vin tous les jours" "=========================")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
Du bon vin
\^@----------\^?

"
	   nil
	   '((?- . simple)
	     nil "Du bon vin" "----------")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
\^@----------\^?
Du bon vin
----------

"
	   nil
	   '((?- . over-and-under)
	     "----------" "Du bon vin" "----------")
	   t))
  (should (equal-buffer-return
	   '(classify-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
--------------
  Du bon vin
\^@--------------\^?

"
	   nil
	   '((?- . over-and-under)
	     "--------------" "  Du bon vin" "--------------")
	   t))
  )
