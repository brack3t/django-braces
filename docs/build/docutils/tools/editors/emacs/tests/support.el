;; Test the test support

(add-to-list 'load-path ".")
(load "ert-support" nil t)

;; ****************************************************************************
;; `buf'

(defun roundtrip-buf (in)
  (with-temp-buffer
    (buf2buffer (string2buf in))
    (buf-string (buffer2buf))))

(ert-deftest buf ()
  "Tests for functions working with `buf's"
  (should (equal (concat buf-point-char "abc\n")
		 (roundtrip-buf (concat buf-point-char "abc\n"))))
  (should (equal (concat "a" buf-point-char "bc\n")
		 (roundtrip-buf (concat "a" buf-point-char "bc\n"))))
  (should (equal (concat "ab" buf-point-char "c\n")
		 (roundtrip-buf (concat "ab" buf-point-char "c\n"))))
  (should (equal (concat "abc" buf-point-char "\n")
		 (roundtrip-buf (concat "abc" buf-point-char "\n"))))
  (should (equal (concat "abc\n" buf-point-char)
		 (roundtrip-buf (concat "abc\n" buf-point-char))))
  (should (equal (concat buf-point-char "abc\n" buf-mark-char "")
		 (roundtrip-buf (concat buf-point-char "abc\n" buf-mark-char ""))))
  (should (equal (concat buf-mark-char "abc\n" buf-point-char)
		 (roundtrip-buf (concat buf-mark-char "abc\n" buf-point-char))))
  (should (equal (concat "a" buf-mark-char buf-point-char "bc\n")
		 (roundtrip-buf (concat "a" buf-point-char "" buf-mark-char "bc\n"))))
  (should (equal (concat "ab" buf-mark-char "" buf-point-char "c\n")
		 (roundtrip-buf (concat "ab" buf-mark-char buf-point-char "c\n"))))
  (should-error (string2buf (concat "ab" buf-point-char buf-point-char "c\n")))
  (should-error (string2buf (concat "ab" buf-mark-char buf-mark-char "c\n")))
  )

;; ****************************************************************************
;; Advice `ert-completing-read'

(defvar read-fun-args nil
  "A list of functions and their argument lists for functions
reading the minibuffer to be run successively. Prompt is omitted.")

(defun insert-reads ()
  (interactive)
  (while read-fun-args
    (let* ((fun-arg (pop read-fun-args))
	   (result (apply (car fun-arg) "" (cdr fun-arg))))
      (insert (if (integerp result)
		  (int-to-string result)
		result) "\n"))))

(defun test-reads (inputs fun-args result)
  (setq read-fun-args fun-args)
  (equal-buffer '(insert-reads) "" result inputs))

(ert-deftest reads ()
  "Tests for functions using `completing-read's"
  (should (test-reads '(5) '((read-number)) "5\n"))
  (should (test-reads nil nil ""))
  (should-error (test-reads '("") nil "")) ;; Too much input
  (should-error (test-reads '(5) '((read-number)
				   (read-number)) "")) ;; Too less input
  (should (test-reads '("") '((completing-read nil)) "\n"))
  (should (test-reads '("" "") '((completing-read nil)
				 (completing-read nil)) "\n\n"))
  (should (test-reads '("a" "b") '((completing-read nil)
				   (completing-read nil)) "a\nb\n"))
  (should (test-reads '("a" "b") '((completing-read ("a" "b"))
				   (completing-read ("a" "b"))) "a\nb\n"))
  (should (test-reads '("a" "b") '((completing-read ("a" "b"))
				   (completing-read ("a"))) "a\nb\n"))
  (should-error (test-reads '("a" "b")
			    '((completing-read ("a" "b"))
			      (completing-read ("a") nil t)) "a\nb\n")) ;; Invalid input
  (should (test-reads '("a" "")
		      '((completing-read ("a" "b"))
			(completing-read ("a") nil t)) "a\n\n"))
  (should-error (test-reads '("a" "")
			    '((completing-read ("a" "b"))
			      (completing-read ("a") nil 'non-empty)) "a\n\n"))
  (should (test-reads '("x") '((read-string)) "x\n"))
  (should (test-reads '("") '((read-string nil nil "x")) "x\n"))
  (should (test-reads '("y") '((read-string nil nil "x")) "y\n"))
  (should (test-reads '("") '((read-number 5)) "5\n"))
  (should (test-reads '(0) '((read-number 5)) "0\n"))
  )
