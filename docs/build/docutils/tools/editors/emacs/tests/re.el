;; Tests for the regular expression builder

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-re ()
  "Tests `rst-re'."
  (should (equal (rst-re "xy") "xy"))
  (should (equal (rst-re ?A) "A"))
  (should (equal (rst-re ?$) "\\$"))
  (should (equal (rst-re 'exm-tag) "\\.\\."))
  (should (equal (rst-re "xy" ?A ?$ 'exm-tag) "xyA\\$\\.\\."))
  (should (equal (rst-re '(:seq "xy" ?A ?$ exm-tag)) "xyA\\$\\.\\."))
  (should (equal (rst-re '(:shy "xy" ?A ?$ exm-tag)) "\\(?:xyA\\$\\.\\.\\)"))
  (should (equal (rst-re '(:grp "xy" ?A ?$ exm-tag)) "\\(xyA\\$\\.\\.\\)"))
  (should (equal (rst-re '(:alt "xy" ?A ?$ exm-tag))
		 "\\(?:xy\\|A\\|\\$\\|\\.\\.\\)"))
  (should (equal (rst-re '(:seq (:seq "xy" ?A ?$ exm-tag))) "xyA\\$\\.\\."))
  (should (equal (rst-re '(:grp (:alt "xy" ?A ?$ exm-tag)))
		 "\\(\\(?:xy\\|A\\|\\$\\|\\.\\.\\)\\)"))
  (should (equal (rst-re '(:alt (:grp "xy" ?A ?$ exm-tag)))
		 "\\(?:\\(xyA\\$\\.\\.\\)\\)"))
  (should (equal (rst-re '(:alt "xy" ?A) '(:grp ?$ exm-tag))
		 "\\(?:xy\\|A\\)\\(\\$\\.\\.\\)"))
  (should-error (rst-re '(:unknown "xy")))
  (should-error (rst-re [1]))
  )

(defun re-equal-modify-orig (orig loc refactored repls)
  (let ((case-fold-search nil))
    (while (string-match "\\[ \t]" orig)
      ;; Transpose horizontal whitespace
      (setq orig (replace-match "[\t ]" t nil orig)))
    (while (string-match "\\\\s \\*\\$" orig)
      ;; Replace symbolic whitespace
      (setq orig (replace-match "[\t ]*$" t nil orig)))
    (dolist (regex-repl repls)
      (if (string-match (car regex-repl) orig)
	  (setq orig (replace-match (cdr regex-repl) t t orig))
	(error "Replacement regex /%s/ didn't match in '%s' for location '%s'"
	       (car regex-repl) orig loc)))
    orig))
  

(defun re-equal (orig loc refactored &rest repls)
  "Compare regex ORIG at location LOC to REFACTORED.
REPLS starts with a list of cons cells telling where regex in car
is replaced by cdr in ORIG."
  (equal (re-equal-modify-orig orig loc refactored repls) refactored))

(defun re-equal-explain (orig loc refactored &rest repls)
  (setq orig (re-equal-modify-orig orig loc refactored repls))
  (ert--explain-not-equal orig refactored))

(put 're-equal 'ert-explainer 're-equal-explain)

(defun re-check-matches (orig loc refactored s pairs)
  "Check matches and return those pairs which didn't work"
  (let ((case-fold-search nil)
	failed)
    (dolist (pair pairs failed)
      (let ((orig-mtc (if (string-match orig s)
			  (match-string (car pair) s)))
	    (refa-mtc (if (string-match refactored s)
			  (match-string (cdr pair) s))))
	(if (not orig-mtc)
	    (error "Original regex '%s' didn't match string '%s' for location '%s'"
		   orig s loc)
	  (if (not refa-mtc)
	      (error "Refactored regex '%s' didn't match string '%s' for location '%s'"
		     refactored s loc)
	    (if (not (equal orig-mtc refa-mtc))
		(push pair failed))))))))

(defun re-equal-matches (orig loc refactored matches &rest repls)
  "Like `re-equal'. However, if MATCHES is non-nil it must be a
list with a string and cons cells each consisting of two numbers.
For each cons cell the string is matched with ORIG and REFACTORED
and the numbered matches are compared."
  (and (equal (re-equal-modify-orig orig loc refactored repls) refactored)
       (not (re-check-matches orig loc refactored
			      (car matches) (cdr matches)))))

(defun re-equal-matches-explain (orig loc refactored matches &rest repls)
  (if (not (equal (re-equal-modify-orig orig loc refactored repls) refactored))
      (apply 're-equal-explain orig loc refactored repls)
    (let (result)
      (dolist (failed (re-check-matches orig loc refactored
					(car matches) (cdr matches)) result)
	(push (list 'matchers-didnt-match (car failed) (cdr failed))
	      result)))))

(put 're-equal-matches 'ert-explainer 're-equal-matches-explain)

(ert-deftest rst-re-refactoring ()
  "Test the refactorings done based on rst_el_1_68."
  ;; Any comment or string "=== rst.el.~rst_el_1_68~:..." gives the line number
  ;; of the refactored code in the original file for the previous expression.
  (let* ((rst-bullets
	  '(?- ?* ?+))
	 ;; === rst.el.~rst_el_1_68~:451
	 (rst-re-bullets
	  (format "\\([%s][ \t]\\)[^ \t]" (regexp-quote (concat rst-bullets))))
	 ;; === rst.el.~rst_el_1_68~:1604
	 ;; More parameters
	 (c ?*)
	 (len 10)
	 (char ?+)
	 (adornment "$$$$")
	 (ado-re (regexp-quote adornment))
	 (fromchar ?.)
	 )
    (should (re-equal
	     "^\\.\\. "
	     "=== rst.el.~rst_el_1_68~:398"
	     (rst-re "^" 'exm-sta)
	     (cons " $" "[	 ]+") ;; Any whitespace may follow
	     ))
    (should (re-equal
	     "^[ \t]*\\S *\\w\\S *"
	     "=== rst.el.~rst_el_1_68~:567"
	     (rst-re 'lin-beg "\\S *\\w\\S *")
	     ))
    (should (re-equal
	     "::[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:591"
	     (rst-re 'dcl-tag 'lin-end)
	     ))
    (should (re-equal
	     "\\.\\.\\.[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:592"
	     (rst-re 'ell-tag 'lin-end)
	     ))
    (should (re-equal
	     ".[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:594"
	     (rst-re "." 'lin-end)
	     ))
    (should (re-equal
	     "^[ \t]+"
	     "=== rst.el.~rst_el_1_68~:605"
	     (rst-re 'hws-sta)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "^[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:744"
	     (rst-re 'lin-end)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "^[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:1517"
	     (rst-re 'lin-end)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "^[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:1545"
	     (rst-re 'lin-end)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "^[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:1548"
	     (rst-re 'lin-end)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "[0-9]+"
	     "=== rst.el.~rst_el_1_68~:1657"
	     (rst-re 'num-tag)
	     ))
    (should (re-equal
	     "[IVXLCDMivxlcdm]+"
	     "=== rst.el.~rst_el_1_68~:1661"
	     (rst-re 'rom-tag)
	     ))
    (should (re-equal
	     "[IVXLCDMivxlcdm]+"
	     "=== rst.el.~rst_el_1_68~:1677"
	     (rst-re 'rom-tag)
	     ))
    (should (re-equal
	     "[a-zA-Z]"
	     "=== rst.el.~rst_el_1_68~:1685"
	     (rst-re 'ltr-tag)
	     ))
    (should (re-equal
	     "[ \t\n]*\\'"
	     "=== rst.el.~rst_el_1_68~:1762"
	     (rst-re "[ \t\n]*\\'")
	     ))
    (should (re-equal
	     "\\S .*\\S "
	     "=== rst.el.~rst_el_1_68~:1767"
	     (rst-re "\\S .*\\S ")
	     ))
    (should (re-equal
	     "[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2066"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2366"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "^[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2414"
	     (rst-re 'lin-end)
	     (cons "^^" "") ;; No need to anchor for looking-at
	     ))
    (should (re-equal
	     "[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2610"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2612"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "[ \t]*$"
	     "=== rst.el.~rst_el_1_68~:2645"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "[^ \t]\\|[ \t]*\\.\\.[^ \t]\\|.*::$"
	     "=== rst.el.~rst_el_1_68~:3177"
	     (rst-re '(:alt
		       "[^ \t]"
		       (:seq hws-tag exm-tag "[^ \t]")
		       (:seq ".*" dcl-tag lin-end)))
	     (cons "^" "\\(?:") (cons "$" "\\)") ;; Add outermost shy group
	     (cons "::\\$" "::[	 ]*$") ;; Allow trailing space after double
				       ;; colon
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3209"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "^\\s *$"
	     "=== rst.el.~rst_el_1_68~:3215"
	     (rst-re 'linemp-tag)
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3253"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "\\s *"
	     "=== rst.el.~rst_el_1_68~:3256"
	     (rst-re 'hws-tag)
	     (cons "\\\\s \\*" "[\t ]*") ;; Replace symbolic by real whitespace
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3261"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "\\s *"
	     "=== rst.el.~rst_el_1_68~:3268"
	     (rst-re 'hws-tag)
	     (cons "\\\\s \\*" "[\t ]*") ;; Replace symbolic by real whitespace
	     ))
    (should (re-equal
	     (regexp-quote adornment)
	     "=== rst.el.~rst_el_1_68~:3346"
	     (rst-re (regexp-quote adornment))
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3354"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3358"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     "\\s *$"
	     "=== rst.el.~rst_el_1_68~:3374"
	     (rst-re 'lin-end)
	     ))
    (should (re-equal
	     (concat "\\(" ado-re "\\)\\s *$")
	     "=== rst.el.~rst_el_1_68~:3377"
	     (rst-re (list :grp
			   ado-re)
		     'lin-end)
	     ))
    (should (re-equal
	     (concat "\\(" ado-re "\\)\\s *$")
	     "=== rst.el.~rst_el_1_68~:3393"
	     (rst-re (list :grp
			   ado-re)
		     'lin-end)
	     ))
    (should (re-equal
	     (concat "^" (regexp-quote (string fromchar)) "+\\( *\\)$")
	     "=== rst.el.~rst_el_1_68~:3590"
	     (rst-re "^" fromchar "+\\( *\\)$")
	     ))
    (should (re-equal
	     "\f\\|>*[ \t]*$\\|>*[ \t]*[-+*] \\|>*[ \t]*[0-9#]+\\. "
	     "=== rst.el.~rst_el_1_68~:391"
	     (rst-re '(:alt
		       "\f"
		       lin-end
		       (:seq hws-tag itmany-sta-1)))
	     (cons "^" "\\(?:") (cons "$" "\\)") ;; Add outermost shy group
	     (cons (regexp-quote "[-+*] \\|>*[	 ]*[0-9#]+\\. ")
		   "\\(\\(?:\\(?:\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\)\\.\\|(?\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\))\\)\\|[-*+\u2022\u2023\u2043]\\)\\)[	 ]+"
		   ) ;; Now matches all items
	     (cons ">\\*" "") ;; Remove ">" prefix
	     (cons ">\\*" "") ;; Remove another ">" prefix
	     (cons "\\[\t ]\\+" "\\(?:[\t ]+\\|$\\)") ;; Item tag needs no
						      ;; trailing whitespace
	     ))
    (should (re-equal
	     (format "[%s]+[ \t]*$" (char-to-string c))
	     "=== rst.el.~rst_el_1_68~:587"
	     (rst-re c "+" 'lin-end)
	     (cons "\\[\\*]" "\\*") ;; Use quoting instead of char class
	     ))
    (should (re-equal
	     (concat "^"
		     (regexp-quote (make-string len char))
		     "$")
	     "=== rst.el.~rst_el_1_68~:941"
	     (rst-re "^" char (format "\\{%d\\}" len) "$")
	     (cons "\\(\\\\\\+\\)\\{9\\}\\$"
		   "\\{10\\}$") ;; Use regex repeat instead of explicit repeat
	     ))
    (should (re-equal
	     (format "#.\\|[%s]"
		     (regexp-quote (concat rst-bullets)))
	     "=== rst.el.~rst_el_1_68~:1653"
	     (rst-re '(:alt
		       enmaut-tag
		       bul-tag))
	     (cons "^" "\\(?:") (cons "$" "\\)") ;; Add outermost shy group
	     (cons (regexp-quote "[-\\*\\+]")
		   "[-*+\u2022\u2023\u2043]") ;; Wrongly quoted characters in
					      ;; class and more bullets
	     (cons "#\\." "\\(?:#\\.\\|(?#)\\)") ;; Use all auto enumerators
	     ))
    (should (re-equal
	     "[a-zA-Z]+"
	     "=== rst.el.~rst_el_1_68~:1672"
	     (rst-re 'ltr-tag)
	     (cons "\\+$" "") ;; Wrong in source
	     ))
    (should (re-equal
	     rst-re-bullets
	     "=== rst.el.~rst_el_1_68~:1735"
	     (rst-re 'bul-sta)
	     (cons (regexp-quote "[-\\*\\+]")
		   "[-*+\u2022\u2023\u2043]") ;; Wrongly quoted characters in
					      ;; class and more bullets
	     (cons "\\[^ \t]" "") ;; Accept bullets without content
	     (cons "^\\\\(" "") (cons "\\\\)" "") ;; Remove superfluous group
	     (cons "$" "+") ;; Allow more whitespace
	     (cons "\\[\t ]\\+" "\\(?:[\t ]+\\|$\\)") ;; Item tag needs no
						      ;; trailing whitespace
	     ))
    (should (re-equal
	     "^\\.\\. contents[ \t]*::\\(.*\\)\n\\([ \t]+:\\w+:.*\n\\)*\\.\\."
	     "=== rst.el.~rst_el_1_68~:2056"
	     (rst-re "^" 'exm-sta "contents" 'dcl-tag ".*\n"
		     "\\(?:" 'hws-sta 'fld-tag ".*\n\\)*" 'exm-tag)
	     (cons " contents\\[\t ]\\*"
		   "[\t ]+contents") ;; Any whitespace before but no after
	     (cons "\\\\(\\.\\*\\\\)" ".*") ;; Remove superfluous group
	     (cons "\\\\(" "\\(?:") ;; Make group shy
	     (cons ":\\\\w\\+:" ":\\(?:[^:\n]\\|\\\\:\\)+:") ;; Use improved
							     ;; field tag
	     ))
    (should (re-equal
	     "[ \t]+[^ \t]"
	     "=== rst.el.~rst_el_1_68~:2065"
	     (rst-re 'hws-sta "\\S ")
	     (cons "\\[^ \t]" "\\S ") ;; Require non-whitespace instead
				      ;; of only non-horizontal whitespace
	     ))
    ))

(ert-deftest rst-re-refactoring-complicated ()
  "Try to test complicated refactorings done based on rst_el_1_68."
  :expected-result :failed ;; These have been reviewed logically and are ok
  (let* ((rst-bullets
	  '(?- ?* ?+))
	 ;; === rst.el.~rst_el_1_68~:451
	 (rst-re-enumerator "\\(?:[a-zA-Z]\\|[0-9IVXLCDMivxlcdm]+\\)")
	 ;; === rst.el.~rst_el_1_68~:1608
	 (rst-re-enumerations
	  (format "^[ \t]*\\(%s.\\|(?%s)\\)[ \t]"
		  rst-re-enumerator
		  rst-re-enumerator))
	 ;; === rst.el.~rst_el_1_68~:1610
	 (rst-re-items
	  (format "^[ \t]*\\([%s]\\|\\(#\\|%s\\)\\.\\|(?%s)\\)[ \t]"
		  (regexp-quote (concat rst-bullets))
		  rst-re-enumerator
		  rst-re-enumerator))
	 ;; === rst.el.~rst_el_1_68~:1616
	 ;; More parameters
	 (char ?+)
	 )
    (should (re-equal
	     rst-re-items
	     "=== rst.el.~rst_el_1_68~:2718"
	     (rst-re 'itmany-sta-1)
	     (cons "^^\\[\t ]\\*" "") ;; Wrongly anchored at the beginning of
				      ;; the line
	     (cons (regexp-quote "\\(#\\|\\(?:[a-zA-Z]\\|[0-9IVXLCDMivxlcdm]+\\)\\)")
		   "\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\)"
		   ) ;; Replace counter for "\\."
	     (cons (regexp-quote "\\(?:[a-zA-Z]\\|[0-9IVXLCDMivxlcdm]+\\)")
		   "\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\)"
		   ) ;; Replace counter for "(?)"
	     (cons "$" "+") ;; Allow more whitespace
	     (cons "^\\\\(" "\\(\\(?:") (cons "\\[\t ]\\+$" "\\)[\t ]+"
					      ) ;; Add superfluous shy group
	     (cons (regexp-quote "[-\\*\\+]\\|") "") ;; Remove wrongly quoted
						     ;; characters
	     (cons (regexp-quote "\\)\\)[\t ]+")
		   "\\|[-*+\u2022\u2023\u2043]\\)\\)[\t ]+"
		   ) ;; Re-add bullets
	     ))
    (should (re-equal
	     rst-re-items
	     "=== rst.el.~rst_el_1_68~:2724"
	     (rst-re 'itmany-beg-1)
	     ))
    (should (re-equal
	     rst-re-items
	     "=== rst.el.~rst_el_1_68~:1649"
	     (rst-re 'itmany-beg-1)
	     ))
    (should (re-equal
	     rst-re-enumerations
	     "=== rst.el.~rst_el_1_68~:1671"
	     (rst-re 'enmexp-beg)
	     ))
    (should (re-equal
	     rst-re-items
	     "=== rst.el.~rst_el_1_68~:1719"
	     (rst-re 'itmany-beg-1)
	     ))
    (should (re-equal
	     (concat
	      "\\(?:"
	      "\\(\\(?:[0-9a-zA-Z#]\\{1,3\\}[.):-]\\|[*+-]\\)[ \t]+\\)[^ \t\n]"
	      "\\|"
	      (format "\\(%s%s+[ \t]+\\)[^ \t\n]"
		      (regexp-quote (char-to-string char))
		      (regexp-quote (char-to-string char)))
	      "\\)")
	     "=== rst.el.~rst_el_1_68~:2430"
	     (rst-re
	      `(:grp
		(:alt
		 itmany-tag
		 (:seq ,(char-after) "\\{2,\\}"))
		hws-sta)
	      "\\S ")
	     (cons "^\\\\(\\?:" "") (cons "\\\\)$" "") ;; Remove superfluous
						       ;; shy group
	     (cons (regexp-quote "[0-9a-zA-Z#]\\{1,3\\}[.):-]\\|[*+-]")
		   "\\(\\(?:\\(?:\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\)\\.\\|(?\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\))\\)\\|[-*+\u2022\u2023\u2043]\\)\\)"
		   ) ;; Replace wrong item tag by correct one
	     (cons (regexp-quote "\\+\\++")
		   "\\+\\{2,\\}") ;; Use regex repeat instead of explicit repeat
	     (cons "\\[^ \t\n]" "\\S ") ;; Use symbolic non-whitespace
	     (cons "\\[^ \t\n]" "\\S ") ;; Use symbolic non-whitespace again
	     (cons "\\\\S " "") ;; Factor out symbolic non-whitespace
	     ))
    ))

(ert-deftest rst-re-refactoring-font-lock ()
  "Test the refactorings in font-lock done based on rst_el_1_68."
  ;; Any comment or string "=== rst.el.~rst_el_1_68~:..." gives the line number
  ;; of the refactored code in the original file for the previous expression.
  (let* ((rst-use-char-classes t)
	 (rst-use-unicode t)
	 ;; horizontal white space
	 (re-hws "[\t ]")
	 ;; beginning of line with possible indentation
	 (re-bol (concat "^" re-hws "*"))
	 ;; Separates block lead-ins from their content
	 (re-blksep1 (concat "\\(" re-hws "+\\|$\\)"))
	 ;; explicit markup tag
	 (re-emt "\\.\\.")
	 ;; explicit markup start
	 (re-ems (concat re-emt re-hws "+"))
	 ;; inline markup prefix
	 (re-imp1 (concat "\\(^\\|" re-hws "\\|[-'\"([{<"
			  (if rst-use-unicode
			      "\u2018\u201c\u00ab\u2019"
			    "")
			  "/:]\\)"))
	 ;; inline markup suffix
	 (re-ims1 (concat "\\(" re-hws "\\|[]-'\")}>"
			  (if rst-use-unicode
			      "\u2019\u201d\u00bb"
			    "")
			  "/:.,;!?\\]\\|$\\)"))
	 ;; symbol character
	 (re-sym1 "\\(\\sw\\|\\s_\\)")
	 ;; inline markup content begin
	 (re-imbeg2 "\\(\\S \\|\\S \\([^")

	 ;; There seems to be a bug leading to error "Stack overflow in regexp
	 ;; matcher" when "|" or "\\*" are the characters searched for
	 (re-imendbegbeg
	  (if (< emacs-major-version 21)
	      "]"
	    "\\]\\|\\\\."))
	 ;; inline markup content end
	 (re-imendbeg (concat re-imendbegbeg "\\)\\{0,"
			   (format "%d" rst-max-inline-length)
			   "\\}[^\t "))
	 (re-imendend "\\\\]\\)")
	 ;; inline markup content without asterisk
	 (re-ima2 (concat re-imbeg2 "*" re-imendbeg "*" re-imendend))
	 ;; inline markup content without backquote
	 (re-imb2 (concat re-imbeg2 "`" re-imendbeg "`" re-imendend))
	 ;; inline markup content without vertical bar
	 (re-imv2 (concat re-imbeg2 "|" re-imendbeg "|" re-imendend))
	 ;; Supported URI schemes
	 (re-uris1 "\\(acap\\|cid\\|data\\|dav\\|fax\\|file\\|ftp\\|gopher\\|http\\|https\\|imap\\|ldap\\|mailto\\|mid\\|modem\\|news\\|nfs\\|nntp\\|pop\\|prospero\\|rtsp\\|service\\|sip\\|tel\\|telnet\\|tip\\|urn\\|vemmi\\|wais\\)")
	 ;; Line starting with adornment and optional whitespace; complete
	 ;; adornment is in (match-string 1); there must be at least 3
	 ;; characters because otherwise explicit markup start would be
	 ;; recognized
	 (re-ado2 (concat "^\\(\\(["
			  (if rst-use-char-classes
			      "^[:word:][:space:][:cntrl:]"
			    "^\\w \t\x00-\x1F")
			  "]\\)\\2\\2+\\)" re-hws "*$"))
	 )
    (should (re-equal-matches
	     ;; `Bullet Lists`_
	     (concat re-bol "\\([-*+]" re-blksep1 "\\)")
	     "=== rst.el.~rst_el_1_68~:3011"	      
	     (rst-re 'lin-beg '(:grp bul-sta))
	     (list "*"
		   (cons 1 1))
	     (cons (regexp-quote "[-*+]")
		   "[-*+\u2022\u2023\u2043]") ;; More bullets
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Enumerated Lists`_
	     (concat re-bol "\\((?\\(#\\|[0-9]+\\|[A-Za-z]\\|[IVXLCMivxlcm]+\\)[.)]"
		     re-blksep1 "\\)")
	     "=== rst.el.~rst_el_1_68~:3015"
	     (rst-re 'lin-beg '(:grp enmany-sta))
	     (list "  (#) Item"
		   (cons 1 1))
	     (cons (regexp-quote
		    "(?\\(#\\|[0-9]+\\|[A-Za-z]\\|[IVXLCMivxlcm]+\\)[.)]")
		   "\\(?:\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\)\\.\\|(?\\(?:[a-zA-Z]\\|[0-9]+\\|[IVXLCDMivxlcdm]+\\|#\\))\\)"
		   ) ;; Enumeration tags are more sophisticated
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Field Lists`_
	     (concat re-bol "\\(:[^:\n]+:\\)" re-blksep1)
	     "=== rst.el.~rst_el_1_68~:3021"
	     (rst-re 'lin-beg '(:grp fld-tag) 'bli-sfx)
	     (list "  :some field: "
		   (cons 1 1))
	     (cons "\\[^:\n]" "\\(?:[^:\n]\\|\\\\:\\)") ;; Field name more
							;; sophisticated
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Option Lists`_
	     (concat re-bol "\\(\\(\\(\\([-+/]\\|--\\)\\sw\\(-\\|\\sw\\)*"
		     "\\([ =]\\S +\\)?\\)\\(,[\t ]\\)?\\)+\\)\\($\\|[\t ]\\{2\\}\\)")
	     "=== rst.el.~rst_el_1_68~:3025"
	     (rst-re 'lin-beg '(:grp opt-tag (:shy optsep-tag opt-tag) "*")
		     '(:alt "$" (:seq hws-prt "\\{2\\}")))
	     (list "  --len=length, -l length    Explanation"
		   (cons 1 1))
	     (cons (regexp-quote "\\(\\(\\(\\([-+/]\\|--\\)\\sw\\(-\\|\\sw\\)*\\([ =]\\S +\\)?\\)\\(,[	 ]\\)?\\)+\\)")
		   "\\(\\(?:\\(?:[-+/]\\|--\\)\\sw\\(?:-\\|\\sw\\)*\\(?:[ =]\\S +\\)?\\)\\(?:\\(?:,[	 ]\\)\\(?:\\(?:[-+/]\\|--\\)\\sw\\(?:-\\|\\sw\\)*\\(?:[ =]\\S +\\)?\\)\\)*\\)"
		   ) ;; Option recognition more sophisticated
	     (cons "\\\\(\\$" "\\(?:$") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Line Blocks`_
	     (concat re-bol "\\(|" re-blksep1 "\\)[^|\n]*$")
	     "=== rst.el.~rst_el_1_68~:3030"
	     (rst-re 'lin-beg '(:grp "|" bli-sfx) "[^|\n]*$")
	     (list "  | Some text"
		   (cons 1 1))
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Footnotes`_ / `Citations`_
	     (concat re-bol "\\(" re-ems "\\[[^[\n]+\\]\\)" re-blksep1)
	     "=== rst.el.~rst_el_1_68~:3038"
	     (rst-re 'lin-beg '(:grp exm-sta fnc-tag) 'bli-sfx)
	     (list ".. [#]"
		   (cons 1 1))
	     (cons "\\[^\\[" "[^]") ;; Error correction in old code
	     (cons "\\\\]" "]") ;; Remove superfluous quote
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Directives`_ / `Substitution Definitions`_
	     (concat re-bol "\\(" re-ems "\\)\\(\\(|[^|\n]+|[\t ]+\\)?\\)\\("
		     re-sym1 "+::\\)" re-blksep1)
	     "=== rst.el.~rst_el_1_68~:3042"
	     (rst-re 'lin-beg '(:grp exm-sta)
		     '(:grp (:shy subdef-tag hws-sta) "?")
		     '(:grp sym-tag dcl-tag) 'bli-sfx)
	     (list ".. |attr| replace:: val"
		   (cons 1 1)
		   (cons 2 2)
		   (cons 4 3))
	     (cons "\\\\(|" "\\(?:|") ;; Make a group shy
	     (cons "\\[^|\n]\\+" "\\(?:\\S \\|\\S \\(?:[^|\\\n]\\|\\\\.\\)\\{0,1000\\}[^	 |\\]\\)"
		   ) ;; Symbol name more sophisticated
	     (cons "\\\\(\\\\s" "\\(?:\\s") ;; Make a group shy
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Hyperlink Targets`_
	     (concat re-bol "\\(" re-ems "_\\([^:\\`\n]\\|\\\\.\\|`[^`\n]+`\\)+:\\)"
		     re-blksep1)
	     "=== rst.el.~rst_el_1_68~:3049"
	     (rst-re 'lin-beg
		     '(:grp exm-sta "_" (:alt
					 (:seq "`" ilcbkqdef-tag "`")
					 (:seq (:alt "[^:\\\n]" "\\\\.") "+")) ":")
		     'bli-sfx)
	     (list ".. _`some\\: target`:"
		   (cons 1 1))
	     (cons (regexp-quote "\\([^:\\`\n]\\|\\\\.\\|`[^`\n]+`\\)+")
		   "\\(?:`\\(?:\\S \\|\\S \\(?:[^`\\\n]\\|\\\\.\\)\\{0,1000\\}[^	 `\\]\\)`\\|\\(?:[^:\\\n]\\|\\\\.\\)+\\)"
		   ) ;; Hyperlink name recognition more sophisticated
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Hyperlink Targets`_
	     (concat re-bol "\\(__\\)" re-blksep1)
	     "=== rst.el.~rst_el_1_68~:3053"
	     (rst-re 'lin-beg '(:grp "__") 'bli-sfx)
	     (list "  __"
		   (cons 1 1))
	     (cons "\\\\(\\[\t " "\\(?:[\t ") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Strong Emphasis`_
	     (concat re-imp1 "\\(\\*\\*" re-ima2 "\\*\\*\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3062"
	     (rst-re 'ilm-pfx '(:grp "\\*\\*" ilcast-tag "\\*\\*") 'ilm-sfx)
	     (list "abc **def** ghi"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Emphasis`_
	     (concat re-imp1 "\\(\\*" re-ima2 "\\*\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3066"
	     (rst-re 'ilm-pfx '(:grp "\\*" ilcast-tag "\\*") 'ilm-sfx)
	     (list "*x*"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Inline Literals`_
	     (concat re-imp1 "\\(``" re-imb2 "``\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3070"
	     (rst-re 'ilm-pfx '(:grp "``" ilcbkq-tag "``") 'ilm-sfx)
	     (list "``co de``"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Inline Internal Targets`_
	     (concat re-imp1 "\\(_`" re-imb2 "`\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3074"
	     (rst-re 'ilm-pfx '(:grp "_`" ilcbkq-tag "`") 'ilm-sfx)
	     (list "_`Inline\ntarget`"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Hyperlink References`_
	     (concat re-imp1 "\\(\\(`" re-imb2 "`\\|\\(\\sw\\(\\sw\\|-\\)+\\sw\\)\\)__?\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3079"
	     (rst-re 'ilm-pfx '(:grp (:alt (:seq "`" ilcbkq-tag "`")
					   (:seq "\\sw" (:alt "\\sw" "-") "+\\sw"))
				     "__?") 'ilm-sfx)
	     (list "<`xxx`__>"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     (cons "\\\\(`" "\\(?:`") ;; Make a group shy
	     (cons "\\\\(\\\\sw" "\\sw")
	     (cons "\\\\sw\\\\)" "\\sw") ;; Remove a group
	     (cons "sw\\\\(\\\\sw" "sw\\(?:\\sw") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Interpreted Text`_
	     (concat re-imp1 "\\(\\(:" re-sym1 "+:\\)?\\)\\(`" re-imb2 "`\\)\\(\\(:"
		     re-sym1 "+:\\)?\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3083"
	     (rst-re 'ilm-pfx '(:grp (:shy ":" sym-tag ":") "?")
		     '(:grp "`" ilcbkq-tag "`")
		     '(:grp (:shy ":" sym-tag ":") "?") 'ilm-sfx)
	     (list "`Interpreted`"
		   (cons 2 1)
		   (cons 5 2)
		   (cons 8 3))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons "\\\\(\\[^" "\\(?:[^") ;; Make a group shy
	     (cons "\\\\(:" "\\(?::") ;; Make a group shy
	     (cons "\\\\(:" "\\(?::") ;; Make a group shy
	     (cons "\\\\(\\\\sw" "\\(?:\\sw") ;; Make a group shy
	     (cons "\\\\(\\\\sw" "\\(?:\\sw") ;; Make a group shy
	     (cons (regexp-quote "\\\\]") "\\]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Footnote References`_ / `Citation References`_
	     (concat re-imp1 "\\(\\[[^]]+\\]_\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3090"
	     (rst-re 'ilm-pfx '(:grp fnc-tag "_") 'ilm-sfx)
	     (list "[1]_"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "]]" "]\n]") ;; A reference may not contain \n
	     (cons "\\\\]" "]") ;; Remove superfluous quote
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Substitution References`_
	     (concat re-imp1 "\\(|" re-imv2 "|\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3094"
	     (rst-re 'ilm-pfx '(:grp sub-tag) 'ilm-sfx)
	     (list "|attr|"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\S" "\\(?:\\S") ;; Make a group shy
	     (cons (regexp-quote "\\([^|") "\\(?:[^|") ;; Make a group shy
	     (cons "\\\\]" "]") ;; Remove superfluous quote
	     (cons "\\\\]" "]") ;; Remove superfluous quote
	     (cons "\\[^|]" "[^|\\]") ;; Improve recognition
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Standalone Hyperlinks`_
	     (concat re-imp1 "\\(" re-uris1 ":\\S +\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3099"
	     (rst-re 'ilm-pfx '(:grp uri-tag ":\\S +") 'ilm-sfx)
	     (list "http://example.com/"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(acap" "\\(?:acap") ;; Make a group shy
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Standalone Hyperlinks`_
	     (concat re-imp1 "\\(" re-sym1 "+@" re-sym1 "+\\)" re-ims1)
	     "=== rst.el.~rst_el_1_68~:3102"
	     (rst-re 'ilm-pfx '(:grp sym-tag "@" sym-tag ) 'ilm-sfx)
	     (list "someone@example"
		   (cons 2 1))
	     (cons "^\\\\(" "\\(?:") ;; Make a group shy
	     (cons "\\\\(\\\\sw" "\\(?:\\sw") ;; Make a group shy
	     (cons "\\\\(\\\\sw" "\\(?:\\sw") ;; Make a group shy
	     (cons (regexp-quote "\\|$") "")
	     (cons (regexp-quote "\\([\t ]")
		   "\\(?:$\\|[\t ]") ;; Move "$" in regex and make a group shy
	     ))
    (should (re-equal
	     ;; Sections_ / Transitions_
	     re-ado2
	     "=== rst.el.~rst_el_1_68~:3109"
	     (rst-re 'ado-beg-2-1)
	     (cons "\\^\\[:word:]\\[:space:]\\[:cntrl:]"
		   "]!\"#$%&'()*+,./:;<=>?@[\\^_`{|}~-") ;; Use real adornment
							 ;; characters
	     (cons "2\\+" "{2,\\}") ;; Use repeat count
	     ))
    (should (re-equal
	     ;; `Comments`_
	     (concat re-bol "\\(" re-ems "\\)\[^[|_\n]\\([^:\n]\\|:\\([^:\n]\\|$\\)\\)*$")
	     "=== rst.el.~rst_el_1_68~:3128"
	     (rst-re 'lin-beg '(:grp exm-sta) "[^\[|_\n]"
		     '(:alt "[^:\n]" (:seq ":" (:alt "[^:\n]" "$"))) "*$")
	     (cons "\\\\(\\[^:" "\\(?:[^:") ;; Make a group shy
	     (cons "\\\\(\\[^:" "\\(?:[^:") ;; Make a group shy
	     ))
    (should (re-equal-matches
	     ;; `Comments`_
	     (concat re-bol "\\(" re-emt "\\)\\(\\s *\\)$")
	     "=== rst.el.~rst_el_1_68~:3135"
	     (rst-re 'lin-beg '(:grp exm-tag) '(:grp hws-tag) "$")
	     (list ".. "
		   (cons 1 1)
		   (cons 2 2))
	     (cons "\\\\s " "[\t ]") ;; Only horizontal space
	     ))
    (should (re-equal-matches
	     ;; `Literal Blocks`_
	     (concat re-bol "\\(\\([^.\n]\\|\\.[^.\n]\\).*\\)?\\(::\\)$")
	     "=== rst.el.~rst_el_1_68~:3145"
	     (rst-re 'lin-beg '(:shy (:alt "[^.\n]" "\\.[^.\n]") ".*") "?"
		     '(:grp dcl-tag) "$")
	     (list "Some text ::"
		   (cons 3 1))
	     (cons "\\\\(\\\\(" "\\(?:\\(?:") ;; Make two groups shy
	     ))
    (should (re-equal-matches
	     ;; `Doctest Blocks`_
	     (concat re-bol "\\(>>>\\|\\.\\.\\.\\)\\(.+\\)")
	     "=== rst.el.~rst_el_1_68~:3154"
	     (rst-re 'lin-beg '(:grp (:alt ">>>" ell-tag)) '(:grp ".+"))
	     (list ">>> content"
		   (cons 1 1)
		   (cons 2 2))
	     (cons ">>>" "\\(?:>>>") (cons "\\.\\\\)" ".\\)\\)") ;; Add a shy
								 ;; group
	     ))
    ))
