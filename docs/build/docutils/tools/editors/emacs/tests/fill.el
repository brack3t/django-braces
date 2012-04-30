;; Tests for functions around filling

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun auto-fill ()
  "Wrapper to do auto fill."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (auto-fill-mode 1)
    (funcall auto-fill-function)))

(ert-deftest auto-fill ()
  "Tests for auto fill."
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
	     '(auto-fill)
	     "
* This is a test with a fill column of 20\^@"
             "
* This is a test
  with a fill column
  of 20\^@"
             ))
    (should (equal-buffer
	     '(auto-fill)
             "
* This is a test
  with a fill column of 20\^@"
             "
* This is a test
  with a fill column
  of 20\^@"
             ))
    (should (equal-buffer
	     '(auto-fill)
	     "
:Field: Does this work for fields?\^@"
	     "
:Field: Does this
	work for
	fields?\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
:Field: Does this
	work for fields?\^@"
	     "
:Field: Does this
	work for
	fields?\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. dir:: Yes, quite fine\^@"
	     "
.. dir:: Yes, quite
	 fine\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. dir:: Yes, quite fine\^@
   :f: Field"
	     "
.. dir:: Yes, quite
	 fine\^@
   :f: Field"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. |s| r:: Dir ectives had problems\^@"
	     "
.. |s| r:: Dir
	   ectives
	   had
	   problems\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. |s| r:: Dir
	   ectives
	   had problems\^@"
	     "
.. |s| r:: Dir
	   ectives
	   had
	   problems\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. [CIT] X cit is citations are also filled\^@"
	     "
.. [CIT] X cit is
	 citations
	 are also
	 filled\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. [CIT] X cit is
	 citations
	 are also filled\^@"
	     "
.. [CIT] X cit is
	 citations
	 are also
	 filled\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
.. Comments should also fill nicely\^@"
	     "
.. Comments should
   also fill nicely\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
Normal text should also fill as expected\^@"
	     "
Normal text should
also fill as
expected\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
  Normal text should also fill as expected\^@"
	     "
  Normal text should
  also fill as
  expected\^@"
	     ))
    (should (equal-buffer
	     '(auto-fill)
	     "
Normal text should also fill \^@as expected"
	     "
Normal text should
also fill \^@as expected"
	     ))
    ))

(defun explicit-fill ()
  "Wrapper for `fill-paragraph'."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (fill-paragraph)
    (untabify (point-min) (point-max))))

(ert-deftest fill-paragraph ()
  "Tests for `fill-paragraph'."
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
             '(explicit-fill)
             "
* This is a test with a fill column of 20\^@
"
             "
* This is a test
  with a fill column
  of 20\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
* This is a test \^@with a fill column
  of 20
"
             "
* This is a test
  \^@with a fill column
  of 20
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
:Field: Does this work for fields?\^@
"
             "
:Field: Does this
        work for
        fields?\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
:Field: Does this work\^@ for
        fields?
"
             "
:Field: Does this
        work\^@ for
        fields?
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. dir:: Yes, quite fine\^@
"
             "
.. dir:: Yes, quite
         fine\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
\^@.. dir:: Yes, quite
         fine
"
             "
\^@.. dir:: Yes, quite
         fine
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. dir:: Yes, quite fine\^@
   :f: Field
"
             "
.. dir:: Yes, quite
         fine\^@
   :f: Field
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. [CIT] X cit is citations are also filled\^@
"
             "
.. [CIT] X cit is
         citations
         are also
         filled\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. [CIT] X cit is
   citations are also filled\^@
"
             "
.. [CIT] X cit is
   citations are
   also filled\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. [CIT] X cit is
     citations are also filled\^@
"
             "
.. [CIT] X cit is
     citations are
     also filled\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. |s| r:: Dir ectives had problems\^@
"
             "
.. |s| r:: Dir
           ectives
           had
           problems\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
\^@.. |s| r:: Dir ectives had problems
"
             "
\^@.. |s| r:: Dir
           ectives
           had
           problems
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
.. |s| r:: Dir
    ectives had problems\^@
"
             "
.. |s| r:: Dir
    ectives had
    problems\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
Normal \^@text should also fill as expected
"
             "
Normal \^@text should
also fill as
expected
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
\^@Normal text should also fill as expected
"
             "
\^@Normal text should
also fill as
expected
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
  Normal text should also fill as expected\^@
"
             "
  Normal text should
  also fill as
  expected\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
Normal text should also fill as expected

Normal text should also fill as expected\^@
"
             "
Normal text should also fill as expected

Normal text should
also fill as
expected\^@
"
             ))
    (should (equal-buffer
             '(explicit-fill)
             "
\^@Normal text should
also fill as
    expected
"
             "
\^@Normal text should
also fill as
expected
"
             ))
    ))

(ert-deftest fill-paragraph-fixme ()
  "Tests for `fill-paragraph'."
  :expected-result :failed
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3)
	(fill-column 20))
    (should (equal-buffer
             '(explicit-fill)
             "
\^@Normal text should
    also fill as
    expected
"
             "
\^@Normal text should
also fill as
expected
"
             ))
    ))

(defun explicit-fill-region ()
  "Wrapper for `fill-region'."
  (let ((fc fill-column))
    (rst-mode)
    (setq fill-column fc)
    (call-interactively 'fill-region)
    (untabify (point-min) (point-max))))

(ert-deftest fill-region ()
  "Tests for `fill-region'."
  (let ((rst-indent-width 2)
        (rst-indent-field 2)
        (rst-indent-literal-normal 3)
        (rst-indent-literal-minimized 2)
        (rst-indent-comment 3)
        (fill-column 20))
    (should (equal-buffer
             '(explicit-fill-region)
             "\^@
* This is a test with a fill column of 20

* This is a test with a fill column
  of 20

:Field: Does this work for fields?

:Field: Does this work for
        fields?

.. dir:: Yes, quite fine

.. dir:: Yes, quite
         fine

.. dir:: Yes, quite fine
   :f: Field for a directive

.. [CIT] X cit is citations are also filled

.. |s| r:: Dir ectives had problems

.. |s| r:: Dir ectives had problems

Normal text should also fill as expected

  Normal text should also fill as expected
\^?"
             "
* This is a test
  with a fill column
  of 20

* This is a test
  with a fill column
  of 20

:Field: Does this
        work for
        fields?

:Field: Does this
        work for
        fields?

.. dir:: Yes, quite
         fine

.. dir:: Yes, quite
         fine

.. dir:: Yes, quite
         fine
   :f: Field for a
       directive

.. [CIT] X cit is
         citations
         are also
         filled

.. |s| r:: Dir
           ectives
           had
           problems

.. |s| r:: Dir
           ectives
           had
           problems

Normal text should
also fill as
expected

  Normal text should
  also fill as
  expected
\^@\^?"
             ))
    (should (equal-buffer
             '(explicit-fill-region)
             "\^@
* This is a test with a fill column of 20
* This is a test with a fill column
  of 20
:Field: Does this work for fields?
:Field: Does this work for
        fields?
.. dir:: Yes, quite fine
.. dir:: Yes, quite
         fine
.. dir:: Yes, quite fine
   :f: Field for a directive
.. [CIT] X cit is citations are also filled
.. |s| r:: Dir ectives had problems
.. |s| r:: Dir ectives had problems

Normal text should also fill as expected

  Normal text should also fill as expected
\^?"
             "
* This is a test
  with a fill column
  of 20
* This is a test
  with a fill column
  of 20
:Field: Does this
        work for
        fields?
:Field: Does this
        work for
        fields?
.. dir:: Yes, quite
         fine
.. dir:: Yes, quite
         fine
.. dir:: Yes, quite
         fine
   :f: Field for a
       directive
.. [CIT] X cit is
         citations
         are also
         filled
.. |s| r:: Dir
           ectives
           had
           problems
.. |s| r:: Dir
           ectives
           had
           problems

Normal text should
also fill as
expected

  Normal text should
  also fill as
  expected
\^@\^?"
             ))
    ))
