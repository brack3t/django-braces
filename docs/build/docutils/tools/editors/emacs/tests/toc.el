;; Tests for operations on toc

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-toc-insert ()
  "Tests `rst-toc-insert'."
  (let ((title "=====
Title
=====

")
	(headers "Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========"))
    ;; Set customizable variables to defaults
    (let ((rst-toc-indent 2)
	  (rst-toc-insert-style 'fixed)
	  (rst-toc-insert-number-separator "  ")
	  (rst-toc-insert-max-level nil))
      ;; Can't identify a title so do nothing - that's actually a (MIS-)FEATURE
      (should (equal-buffer
	       '(rst-toc-insert)
	       (concat buf-point-char headers)
	       (concat buf-point-char headers)))
      ;; Won't work on a section title
      (should (equal-buffer
	       '(rst-toc-insert)
	       (concat title buf-point-char headers)
	       (concat title buf-point-char headers)))
      ;; No indentation
      (should (equal-buffer
	       '(rst-toc-insert)
	       (concat title buf-point-char "\n\n" headers)
	       (concat title "1  Header A
2  Header B
  2.1  Subheader B.a
    2.1.1  SubSubheader B.a.1
  2.2  Subheader B.b
3  Header C" buf-point-char "

" headers)))
      ;; Indentation
      (should (equal-buffer
	       '(rst-toc-insert)
	       (concat title "  " buf-point-char "\n\n" headers)
	       (concat title "  1  Header A
  2  Header B
    2.1  Subheader B.a
      2.1.1  SubSubheader B.a.1
    2.2  Subheader B.b
  3  Header C" buf-point-char "

" headers)))
      ;; Only first level
      (should (equal-buffer
	       '(rst-toc-insert 1)
	       (concat title "  " buf-point-char "\n\n" headers)
	       (concat title "  1  Header A
  2  Header B
  3  Header C" buf-point-char "

" headers)))
      ;; Prefix and indentation
      (should (equal-buffer
	       '(rst-toc-insert)
	       (concat title "..  " buf-point-char "\n\n" headers)
	       (concat title "..  1  Header A
    2  Header B
      2.1  Subheader B.a
        2.1.1  SubSubheader B.a.1
      2.2  Subheader B.b
    3  Header C" buf-point-char "

" headers)))
      )
    ))

(ert-deftest rst-toc-update ()
  "Tests `rst-toc-update'."
  (let ((title "=====
Title
=====

")
	(headers "Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========")
	(contents ".. contents:: Inhalt\n")
	(fields "   :bla: blub\n   :local:\n")
	(old "..  1  Header A
    2  Header B
    3  Header C")
	(new "..
    1  Header A
    2  Header B
      2.1  Subheader B.a
        2.1.1  SubSubheader B.a.1
      2.2  Subheader B.b
    3  Header C")
	)
    ;; Set customizable variables to defaults
    (let ((rst-toc-indent 2)
	  (rst-toc-insert-style 'fixed)
	  (rst-toc-insert-number-separator "  ")
	  (rst-toc-insert-max-level nil))
      (should (equal-buffer
	       '(rst-toc-update)
	       (concat title contents fields old "\n\n" headers buf-point-char)
	       (concat title contents fields new "\n\n" headers buf-point-char)))
      (should (equal-buffer
	       '(rst-toc-update)
	       (concat title contents old "\n\n" headers buf-point-char)
	       (concat title contents new "\n\n" headers buf-point-char)))
      )
    ))

;; More functions to test:
;; * rst-toc
;; * rst-toc-mode-goto-section
