;; Tests for functions around indentation

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun indent-for-tab (&optional count)
  "Wrapper to call `indent-for-tab-command' COUNT times defaulting to 1."
  (setq count (or count 1))
  (rst-mode)
  (dotimes (i count)
    (indent-for-tab-command)))

(ert-deftest indent-for-tab-command ()
  "Tests for `indent-for-tab-command'."
  (let ((rst-indent-width 2)
	(rst-indent-field 2)
	(rst-indent-literal-normal 3)
	(rst-indent-literal-minimized 2)
	(rst-indent-comment 3))
    (should (equal-buffer
	     '(indent-for-tab)
	     "\^@"
	     "\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
\^@"
	     "
* a
  \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  \^@"
	     "
* a
\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a

* b\^@"
	     "
* a

  * b\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a

  * b\^@"
	     "
* a

* b\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a

\^@* b"
	     "
* a

  \^@* b"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a

  \^@* b"
	     "
* a

\^@* b"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
* e
\^@"
	     "
* a
  * b
    XV. c
  * d
* e
  \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
* e
  \^@"
	     "
* a
  * b
    XV. c
  * d
* e
\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
 * e\^@"
	     "
* a
  * b
    XV. c
  * d
    * e\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
    * e\^@"
	     "
* a
  * b
    XV. c
  * d
  * e\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
  * e\^@"
	     "
* a
  * b
    XV. c
  * d
* e\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
* a
  * b
    XV. c
  * d
* e\^@"
	     "
* a
  * b
    XV. c
  * d
    * e\^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
                   \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
                 \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
                 \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
               \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
               \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
             \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
             \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
            \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
            \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
          \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
          \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
        \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
        \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
      \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
      \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
   \^@"
	     ))
    (should (equal-buffer
	     '(indent-for-tab)
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
   \^@"
	     "
.. [CIT]

     citation

   .. |sub| dir:: Same

        * a

          * b

             :f: val::
\^@"
	     ))
  ))
