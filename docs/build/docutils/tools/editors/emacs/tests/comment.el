;; Tests for comment handling

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun cmnt-insert ()
  "Wrapper to insert comment via `comment-dwim'.
Must be called on a line conaining at most whitespace."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (comment-dwim nil)))

(ert-deftest comment-insert ()
  "Tests for inserting a comment."
  (let ((rst-indent-width 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
	     '(cmnt-insert)
	     "\^@"
	     ".. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-insert)
	     "
   \^@"
	     "
.. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-insert)
	     "
* bla

   \^@"
	     "
* bla

  .. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-insert)
	     "
:Field: Content

\^@"
	     "
:Field: Content

	.. \^@"
	     ))
    ))

(defun cmnt-indent (continue)
  "Wrapper for `comment-indent'."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (comment-indent continue)))

(ert-deftest comment-indent ()
  "Tests for `comment-indent'."
  (let ((rst-indent-width 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     "\^@"
	     ".. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     "
   \^@"
	     "
.. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     ".. comment\^@"
	     ".. \^@comment"
	     ))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     "
* bla

.. comment\^@"
	     "
* bla

  .. \^@comment"
	     ))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     "
:Field: Content

\^@"
	     "
:Field: Content

	.. \^@"
	     ))
    (should (equal-buffer
	     '(cmnt-indent nil)
	     "
:Field: Content

.. comment\^@"
	     "
:Field: Content

	.. \^@comment"
	     ))
    ))

(defun uncmnt-region ()
  "Wrapper for `uncomment-region'."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (call-interactively 'uncomment-region)))

(ert-deftest uncomment-region ()
  "Tests for `uncomment-region'."
  (let ((rst-indent-width 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
	     '(uncmnt-region)
	     "\^?..
   com\^@ment"
	     "\^?com\^@ment"
	     ))
    (should (equal-buffer
	     '(uncmnt-region)
	     "\^?..
   com\^@ment

   bla
"
	     "\^?com\^@ment

   bla
"
	     ))
    (should (equal-buffer
	     '(uncmnt-region)
	     "\^?..
   comment

   bl\^@a
"
	     "\^?comment

bl\^@a
"
	     ))
    ))

(defun cmnt-region ()
  "Wrapper for `comment-region'."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (call-interactively 'comment-region)))

(ert-deftest comment-region ()
  "Tests for `comment-region'."
  (let ((rst-indent-width 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
	     '(cmnt-region)
	     "\^?com\^@ment"
	     "\^?..
   com\^@ment"
	     ))
    (should (equal-buffer
	     '(cmnt-region)
	     "\^?com\^@ment

   bla
"
	     "\^?..
   com\^@ment

   bla
"
	     ))
    (should (equal-buffer
	     '(cmnt-region)
	     "\^?comment

bl\^@a
"
	     "\^?..
   comment

   bl\^@a
"
	     ))
    ))

;; comment-kill could be tested but since there are no suffix comments in
;; reStructuredText this makes little sense
