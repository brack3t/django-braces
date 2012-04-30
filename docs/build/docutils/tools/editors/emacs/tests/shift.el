;; Tests for various functions around shifting text

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun string-insert (s col char)
  "Insert CHAR at position COL in S. Return S."
  (let ((l (length s))
	(c (char-to-string char)))
    (if (<= l col)
	(concat s (make-string (- col l) ? ) c)
      (concat (substring s 0 col) c (substring s (1+ col))))))

(defun line-tabs ()
  "Wrapper to call `rst-line-tabs' creating a readable result."
  (let ((tabs (rst-line-tabs))
	(s "")
	(cnt 0)
	c)
    (delete-region (point-min) (point-max))
    (dolist (tab tabs)
      (setq c (+ ?A cnt))
      (if (cdr tab) (setq c (+ c (- ?a ?A))))
      (setq s (string-insert s (car tab) c))
      (setq cnt (1+ cnt)))
    (insert "\n" s)))

(ert-deftest rst-line-tabs ()
  "Tests for `rst-line-tabs'."
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3))
    (should (equal-buffer
	     '(line-tabs)
	     "\^@"
	     "
"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
* a\^@"
	     "
B a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
  * b\^@"
	     "
  B a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
    XV. c\^@"
	     "
    B   a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
* \^@"
	     "
A"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
  *\tb\^@"
	     "
  B     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "Some para\^@"
	     "
A"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "  A quoted block\^@"
	     "
  A"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
:Field: Content on same line\^@"
	     "
C b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
:Field: \^@"
	     "
B a"
	     ))
    (let ((rst-indent-field 0))
      (should (equal-buffer
	       '(line-tabs)
	       "
:Field: Content on same line\^@"
	       "
B       a"
	       ))
      (should (equal-buffer
	       '(line-tabs)
	       "
:Field: \^@"
	       "
B       a"
	       )))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. dir:: Content on same line\^@"
	     "
C  b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. dir:: \^@"
	     "
B  a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. |sub| dir:: Content on same line\^@"
	     "
D  c     b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. |sub| dir::\^@"
	     "
C  b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. [CIT] citation\^@"
	     "
C  b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. [#FN] Footnote\^@"
	     "
C  b     a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. [CIT]\^@"
	     "
B  a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
Some text::\^@"
	     "
B a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
::\^@"
	     "
B  a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
  ::\^@"
	     "
  B  a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
.. First comment\^@"
	     "
B  a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
    XV. c::\^@"
	     "
    C   b a"
	     ))
    (should (equal-buffer
	     '(line-tabs)
	     "
      :f: val::\^@"
	     "
      D c b a"
	     ))
  ))

(defun compute-tabs ()
  "Wrapper to call `rst-compute-tabs' creating a readable result."
  (let ((tabs (rst-compute-tabs (point)))
	(s "")
	(cnt 0))
    (delete-region (point-min) (point-max))
    (dolist (tab tabs)
      (setq s (string-insert s tab (+ ?A cnt)))
      (setq cnt (1+ cnt)))
    (insert "\n" s)))

(ert-deftest rst-compute-tabs ()
  "Tests for `rst-compute-tabs'."
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3))
    (should (equal-buffer
	     '(compute-tabs)
	     "\^@"
	     "
"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
\^@"
	     "
B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
  * b
\^@"
	     "
C B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
  * b
    XV. c
\^@"
	     "
D C B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
    XV. c
\^@"
	     "
D C B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* 
    XV. c
\^@"
	     "
C   B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
  * b
    XV. c
  * d
\^@"
	     "
C B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
  * b
    XV. c
  * d
* e
\^@"
	     "
B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
  * a
    * b
      XV. c
\^@"
	     "
  D C B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
  *\tb
\^@"
	     "
C B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "* a
\^@"
	     "
B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "  * a
    * b
      XV. c
\^@"
	     "
  D C B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "Some para

  A quoted block

     Quoting again
\^@"
	     "
C B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
:Field: Content on same line
\^@"
	     "
C B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
:Field: Content on same line
   but continued differently
\^@"
	     "
D CA    B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
:Field: Content on same line
            but continued differently
\^@"
	     "
D C     B   A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
:Field:

   Content on next line
\^@"
	     "
C BA"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
:Field: Starts on same line

  Content on next line
\^@"
	     "
C A     B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. dir:: Content on same line
\^@"
	     "
C  B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. dir:: 

   Content on next line
\^@"
	     "
B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. dir:: 

     Content on next line
\^@"
	     "
C  B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. dir:: Same

     Content on next line
\^@"
	     "
D  C A   B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Content on same line
\^@"
	     "
D  C     B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir::

   Content on next line
\^@"
	     "
C  A     B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir::

     Content on next line
\^@"
	     "
D  C A   B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

     Content on next line
\^@"
	     "
E  D A   C     B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

    :par: val

      Content on next line
\^@"
	     "
E  DC A   B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir::

    :par: val

   Content on next line
\^@"
	     "
C  A     B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

   :f: val

    Content on next line
\^@"
	     "
E  DAC B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

   :f: val

   Content on next line
\^@"
	     "
D  A C B"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

   :f: val

   * Item
\^@"
	     "
C  B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. |sub| dir:: Same

   :f: val

   * Item

     1. Enumerated
\^@"
	     "
D  C B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT] citation
\^@"
	     "
C  B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [#FN] Footnote
\^@"
	     "
C  B     A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT]

   citation
\^@"
	     "
B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT]

     citation
\^@"
	     "
C  B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

      :f: val

      * Item

	1. Enumerated
\^@"
	     "
E  D  C B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
Some text::
\^@"
	     "
B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
No text

::
\^@"
	     "
B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
No text

  ::
\^@"
	     "
C B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

      :f: val

      No text

	::
\^@"
	     "
E  D  C B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. First comment
\^@"
	     "
B  A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
* a
    XV. c::
\^@"
	     "
E D C   B A"
	     ))
    (should (equal-buffer
	     '(compute-tabs)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

      :f: val::
\^@"
	     "
F  E  D C B A"
	     ))
  ))

(ert-deftest rst-shift-region-right ()
  "Tests for `rst-shift-region' to the right."
  (let ((rst-indent-width 2)) ; Set relevant variables
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "
\^@a
\^?"
	     "
\^@  a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "
\^@  a
\^?"
	     "
\^@    a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "\^@
a
b
\^?"
	     "\^@
  a
  b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^@
a
b
\^?"
	     "*  x
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^@
   a
   b
\^?"
	     "*  x
\^@
     a
     b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^?
a
b
\^@"
	     "*  x
\^?
   a
   b
\^@"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 2)
	     "*  x
\^?
a
b
\^@"
	     "*  x
\^?
     a
     b
\^@"
	     t))
    ))

(ert-deftest rst-shift-region-left ()
  "Tests for `rst-shift-region' to the left."
  (let ((rst-indent-width 2)) ; Set relevant variables
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^@
     a
     b
\^?"
	     "*  x
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "
\^@  a
\^?"
	     "
\^@a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "
\^@    a
\^?"
	     "
\^@  a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "\^@
  a
  b
\^?"
	     "\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^@
   a
   b
\^?"
	     "*  x
\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^?
   a
   b
\^@"
	     "*  x
\^?
a
b
\^@"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 0)
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     "*  x
   *  y
\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "\^@*  x
   *  y

      a
      b
\^?"
	     "\^@*  x
   *  y

      a
      b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -2)
	     "*  x
   *  y
\^@
        a
        b
\^?"
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     t))
    ))
