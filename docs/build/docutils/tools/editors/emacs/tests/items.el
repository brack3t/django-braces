;; Tests for operations on list items

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-convert-bullets-to-enumeration ()
  "Tests `rst-convert-bullets-to-enumeration'."
  (should (equal buf-point-char "\0"))
  (should (equal buf-mark-char "\177"))
  (should (equal-buffer
	   '(rst-convert-bullets-to-enumeration)
"\0Normal paragraph.

* A bullet

* Another bullet

Another normal paragraph.

\177"
"\0Normal paragraph.

1. A bullet

2. Another bullet

Another normal paragraph.

\177" t))
  (should (equal-buffer
	   '(rst-convert-bullets-to-enumeration)
"Normal paragraph.

\177* A bullet

* Another bullet

\0Another normal paragraph.

"
"Normal paragraph.

\1771. A bullet

2. Another bullet

\0Another normal paragraph.

" t))
  (should (equal-buffer
	   '(rst-convert-bullets-to-enumeration)
"Normal paragraph.

\177* A bullet

* Another bullet

1. A bullet

2. Another bullet

\0Another normal paragraph.

"
		      
"Normal paragraph.

\1771. A bullet

2. Another bullet

3. A bullet

4. Another bullet

\0Another normal paragraph.

" t))
  )

(ert-deftest rst-convert-bullets-to-enumeration-BUGS ()
  "Exposes bugs in `rst-convert-bullets-to-enumeration'."
  :expected-result :failed ;; These are bugs
  (should (equal-buffer
	   '(rst-convert-bullets-to-enumeration)
"\0Normal paragraph.

* A bullet

* Another bullet

  * A bullet

  * Another bullet

Another normal paragraph.

\177"
"\0Normal paragraph.

1. A bullet

2. Another bullet

  * A bullet

  * Another bullet

Another normal paragraph.

\177" t))
  )

(ert-deftest rst-insert-list-continue ()
  "Tests `rst-insert-list' when continuing a list."
  (should (equal buf-point-char "\0"))
  (should (equal-buffer
	   '(rst-insert-list)
"* Some text\0\n"
"* Some text
* \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"* Some \0text\n"
"* Some text
* \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"* \0Some text\n"
"* Some text
* \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"* Some text
  - A deeper hyphen bullet\0\n"
"* Some text
  - A deeper hyphen bullet
  - \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"* Some text
  - \0Some text\n"
"* Some text
  - Some text
  - \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"1. Some text\0\n"
"1. Some text
2. \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"2. Some text\0\n"
"2. Some text
3. \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"a) Some text\0\n"
"a) Some text
b) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"(A) Some text\0\n"
"(A) Some text
\(B) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"(I) Some text\0\n"
"(I) Some text
\(J) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"(I) Some text\0\n"
"(I) Some text
\(J) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"(h) Some text
\(i) Some text\0\n"
"(h) Some text
\(i) Some text
\(j) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list t)
"(i) Some text\0\n"
"(i) Some text
\(ii) \0\n"))
  (should (equal-buffer
	   '(rst-insert-list)
"(iv) Some text
\(v) Some text\0\n"
"(iv) Some text
\(v) Some text
\(vi) \0\n"))
  )

(ert-deftest rst-insert-list-continue-BUGS ()
  "Exposes bugs in `rst-insert-list-continue'."
  :expected-result :failed ;; These are bugs
  (should (equal-buffer
	   '(rst-insert-list)
"(iv) Some text

\(v) Some text\0\n"
"(iv) Some text

\(v) Some text
\(vi) \0\n")))

(ert-deftest rst-insert-list-new ()
  "Tests `rst-insert-list' when inserting a new list."
  (should (equal buf-point-char "\0"))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"* \0\n" '("*")))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"- \0\n" '("-")))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"#. \0\n" '("#.")))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"5) \0\n" '("1)" 5)))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"(i) \0\n" '("(i)" "")))
  (should (equal-buffer
	   '(rst-insert-list)
"\0\n"
"IV. \0\n" '("I." 4)))
  (should (equal-buffer
	   '(rst-insert-list)
"Some line\0\n"
"Some line

IV. \0\n" '("I." 4)))
  (should (equal-buffer
	   '(rst-insert-list)
"Some line
\0\n"
"Some line

IV. \0\n" '("I." 4)))
  (should (equal-buffer
	   '(rst-insert-list)
"Some line

\0\n"
"Some line

IV. \0\n" '("I." 4)))
  )
