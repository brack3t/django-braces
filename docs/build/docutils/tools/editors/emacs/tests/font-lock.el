;; Tests for font-locking code

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-forward-indented-block ()
  "Tests `rst-forward-indented-block'."
  (should (equal-buffer-return
	   '(rst-forward-indented-block)
	   "\^@abc"
	   "\^@abc"
	   nil))
  (should (equal-buffer-return
	   '(rst-forward-indented-block)
	   (concat "  \^@abc

def")
	   (concat "  abc
\^@
def")
	   7))
  )

(defun extend-region (beg end)
  "Wrapper for `rst-font-lock-extend-region-internal'.
Uses and sets region and returns t if region has been changed."
  (interactive "r")
  (let ((r (rst-font-lock-extend-region-internal beg end)))
    (when r
      (goto-char (car r))
      (set-mark (cdr r))
      t)))

(ert-deftest rst-font-lock-extend-region-internal ()
  "Tests `rst-font-lock-extend-region-internal'."
  (should (equal-buffer-return
	   '(extend-region)
	   "\^@abc\^?"
	   "\^@abc\^?"
	   nil
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "\^@  abc\^?"
	   "\^@  abc\^?"
	   nil
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "  abc
\^@  def\^?"
	   "\^@  abc
  def\^?"
	   t
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "xyz
abc
\^@  def\^?"
	   "xyz
\^@abc
  def\^?"
	   t
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "xyz
  abc::
\^@  def\^?"
	   "xyz
\^@  abc::
  def\^?"
	   t
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "xyz
  .. abc
\^@     def\^?"
	   "xyz
\^@  .. abc
     def\^?"
	   t
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "xyz
  .. abc
     123
\^@       def\^?"
	   "xyz
\^@  .. abc
     123
       def\^?"
	   t
	   t))
  (should (equal-buffer-return
	   '(extend-region)
	   "xyz

  .. abc

     123

\^@       def\^?"
	   "xyz

\^@  .. abc

     123

       def\^?"
	   t
	   t))
  )
