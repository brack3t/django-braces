;; Tests for various movement commands

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun fwd-para ()
  "Wrapper to call `forward-paragraph'."
  (rst-mode)
  (forward-paragraph))

(ert-deftest forward-paragraph ()
  "Tests for `forward-paragraph'."
  (should (equal-buffer-return
	   '(fwd-para)
	   "
:Field: Content
\^@
  More content
  over several

  * An item
    with multi
"
	   "
:Field: Content

  More content
  over several
\^@
  * An item
    with multi
"
	   0
	   ))


  (should (equal-buffer-return
	   '(fwd-para)
	   "\^@
This is
a short
para"
	   "
This is
a short
para\^@"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "\^@
This is
a short
para
"
	   "
This is
a short
para
\^@"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "\^@
This is
a short
para

"
	   "
This is
a short
para
\^@
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "\^@
This is
a short
para


"
	   "
This is
a short
para
\^@

"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
\^@This is
a short
para

"
	   "
This is
a short
para
\^@
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
This is
a short
\^@para

"
	   "
This is
a short
para
\^@
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
This is
a short
\^@para

"
	   "
This is
a short
para
\^@
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
This is
\^@a short
  para

"
	   "
This is
a short
  para
\^@
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
This is
a short
\^@para

This is
a short
para

"
	   "
This is
a short
para
\^@
This is
a short
para

"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
\^@* An item

* Another item
"
	   "
* An item
\^@
* Another item
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
\^@* An item
* Another item
"
	   "
* An item
\^@* Another item
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
\^@:Field: Content

  More content
  over several

  * An item
    with multi
"
	   "
:Field: Content
\^@
  More content
  over several

  * An item
    with multi
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
:Field: Content
\^@
  More content
  over several

  * An item
    with multi
"
	   "
:Field: Content

  More content
  over several
\^@
  * An item
    with multi
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
:Field: Content

  More content
  over several
\^@
  * An item
    with multi
"
	   "
:Field: Content

  More content
  over several

  * An item
    with multi
\^@"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "\^@
.. |s| d::
  :F: Content

    More content
    over several
  * An item
    with multi
"
	   "
.. |s| d::
\^@  :F: Content

    More content
    over several
  * An item
    with multi
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
.. |s| d::
\^@  :F: Content

    More content
    over several
  * An item
    with multi
"
	   "
.. |s| d::
  :F: Content
\^@
    More content
    over several
  * An item
    with multi
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
.. |s| d::
  :F: Content
\^@
    More content
    over several
  * An item
    with multi
"
	   "
.. |s| d::
  :F: Content

    More content
    over several
\^@  * An item
    with multi
"
	   0
	   ))
  (should (equal-buffer-return
	   '(fwd-para)
	   "
.. |s| d::
  :F: Content

    More content
    over several
\^@  * An item
    with multi
"
	   "
.. |s| d::
  :F: Content

    More content
    over several
  * An item
    with multi
\^@"
	   0
	   ))
  )
