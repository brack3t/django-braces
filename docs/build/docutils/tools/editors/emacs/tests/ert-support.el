;; Common support facilities

(add-to-list 'load-path "..")
(load "rst.el" nil t)

(require 'cl)
(require 'ert)

;; ****************************************************************************
;; ****************************************************************************
;; Support for comparison of buffer results

;; ****************************************************************************
;; `buf' and related functions

(defvar buf-point-char "\^@"
  "Special character used to mark the position of point in a `buf'.")

(defvar buf-mark-char "\^?"
  "Special character used to mark the position of mark in a `buf'.")

(defstruct (buf
	    (:constructor string2buf
			  (string
			   &aux
			   (analysis (buf-parse-string string))
			   (content (car analysis))
			   (point (cadr analysis))
			   (mark (caddr analysis))))
	    (:constructor buffer2buf
			  (&aux
			   (content (buffer-substring-no-properties
				     (point-min) (point-max)))
			   (point (point))
			   (mark (mark t))
			   (string (buf-create-string content point mark)))))
  "Structure to hold comparable information about a buffer."
  (content nil :read-only t)
  (point nil :read-only t)
  (mark nil :read-only t)
  (string nil :read-only t)
  )

(defun buf-parse-string (string)
  "Parse STRING and return a list constisting of the cleaned
content, the position of point if `buf-point-char' was found and
the the position of mark if `buf-mark-char' was found."
  (with-temp-buffer
    (let ((case-fold-search nil)
	  fnd point-fnd mark-fnd)
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward (concat "[" buf-point-char buf-mark-char "]")
				nil t)
	(setq fnd (match-string 0))
	(replace-match "")
	(cond
	 ((equal fnd buf-point-char)
	  (if point-fnd
	      (error "Duplicate point"))
	  (setq point-fnd (point)))
	 ((equal fnd buf-mark-char)
	  (if mark-fnd
	      (error "Duplicate mark"))
	  (setq mark-fnd (point)))
	 (t
	  (error "Unexpected marker found"))))
      (list (buffer-substring-no-properties (point-min) (point-max))
	    point-fnd mark-fnd))))

(defun buf-create-string (content point mark)
  "Creates a string representation from CONTENT, POINT and MARK."
  (with-temp-buffer
    (insert content)
    (let (pnt-chs)
      (if point
	  (setq pnt-chs (nconc pnt-chs (list (cons point buf-point-char)))))
      (if mark
	  (setq pnt-chs (nconc pnt-chs (list (cons mark buf-mark-char)))))
      ;; Sort pairs so the highest position is last
      (setq pnt-chs (sort pnt-chs (lambda (el1 el2) (> (car el1) (car el2)))))
      (while pnt-chs
	(goto-char (caar pnt-chs))
	(insert (cdar pnt-chs))
	(setq pnt-chs (cdr pnt-chs)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun buf2buffer (buf)
  "Set current buffer according to BUF."
  (insert (buf-content buf))
  (if (buf-point buf)
      (goto-char (buf-point buf)))
  (if (buf-mark buf)
      (set-mark (buf-mark buf))))

;; ****************************************************************************
;; Runners

(defvar ert-inputs nil
  "Variable to hold the strings to give successively to `ert-completing-read'.")

(defadvice completing-read (around ert-completing-read first
				   (prompt collection &optional predicate
					   require-match initial-input hist
					   def inherit-input-method))
  "Advice for `completing-read' to accept input from `ert-inputs'
instead of the minibuffer."
  (if (not ert-inputs)
      (error "No more input strings in `ert-inputs'"))
  (let* ((input (pop ert-inputs)))
    (setq ad-return-value
	  (cond
	   ((eq (try-completion input collection predicate) t) ;; Perfect match
	    input)
	   ((not require-match) ;; Non-matching input allowed
	    input)
	   ((and (equal input "")
		 (eq require-match t)) ;; Empty input and this is allowed
	    input)
	   (t
	    (error "Input '%s' is not allowed for `completing-read' expecting %s"
		   input collection))))))

(defadvice read-string (around ert-read-string first
			       (prompt &optional initial-input history
				       default-value inherit-input-method))
  "Advice for `read-string' to accept input from `ert-inputs'
instead of the minibuffer."
  (if (not ert-inputs)
      (error "No more input strings in `ert-inputs'"))
  (let* ((input (pop ert-inputs)))
    (setq ad-return-value
	  (if (and (equal input "") default-value)
	      default-value
	    input))))

(defadvice read-number (around ert-read-number first
			       (prompt &optional default))
  "Advice for `read-number' to accept input from `ert-inputs'
instead of the minibuffer."
  (if (not ert-inputs)
      (error "No more input strings in `ert-inputs'"))
  (let* ((input (pop ert-inputs)))
    (setq ad-return-value
	  (if (and (equal input "") default)
	      default
	    input))))

(defun run-test (input funcall interactive)
  "Run list FUNCALL with a buffer filled with INPUT. Return a
cons consisting of the return value and a `buf'. If INTERACTIVE
is non-nil FUNCALL is called in an interactive environment."
  (let ((buf (string2buf input)))
    (with-temp-buffer
      (buf2buffer buf)
      (let ((act-return
	     (cond
	      ((not interactive)
	       (apply (car funcall) (cdr funcall)))
	      ((eq interactive t)
	       (let ((current-prefix-arg (cadr funcall)))
		 (call-interactively (car funcall))))
	      ((listp interactive)
	       (setq ert-inputs interactive)
	       (ad-activate 'read-string)
	       (ad-activate 'read-number)
	       (ad-activate 'completing-read)
	       (unwind-protect
		   (let ((current-prefix-arg (cadr funcall)))
		     (call-interactively (car funcall)))
		 (progn
		   (ad-deactivate 'completing-read)
		   (ad-deactivate 'read-number)
		   (ad-deactivate 'read-string)))
	       (if ert-inputs
		   (error "%d input strings left over"
			  (length ert-inputs))))))
	    (act-buf (buffer2buf)))
	(cons act-return act-buf)))))

(defun compare-test (result exp-output ignore-return exp-return)
  "Compare the RESULT of a test from `run-test' with expexted values.
Return a list of booleans where t stands for a successful test of this kind:

* Content of output buffer
* Point in output buffer
* Return value"
  (let ((act-return (car result))
	(act-buf (cdr result))
	(exp-buf (and exp-output (string2buf exp-output))))
    (list
     (or (not exp-buf)
	 (equal (buf-content act-buf) (buf-content exp-buf)))
     (or
      (not exp-buf)
      (not (buf-point exp-buf))
      (equal (buf-point act-buf) (buf-point exp-buf)))
     (or ignore-return
	 (equal act-return exp-return)))))

(defun equal-buffer-internal (funcall input exp-output ignore-return exp-return interactive)
  "Run list FUNCALL with a buffer filled with INPUT. Compare the
buffer content to EXP-OUTPUT if this is non-nil and the return
value to EXP-RETURN. Ignore return value if IGNORE-RETURN. Return
t if equal.

INPUT and EXP-OUTPUT are expected to be parsable by
`buf-parse-string'.

If INTERACTIVE is non-nil the FUNCALL is done interactively and
`current-prefix-arg' is set to the cadr of FUNCALL and thus must
comply to the format of `current-prefix-arg'. If INTERACTIVE is t
only `call-interactively' is used. If INTERACTIVE is a list of
strings the elements of the list are given to (advised forms of)
functions reading from the minibuffer as user input strings."
  (reduce (lambda (l r) (and l r))
	  (compare-test (run-test input funcall interactive)
			exp-output ignore-return exp-return)))

(defun equal-buffer-return (funcall input exp-output exp-return &optional interactive)
  "Call `equal-buffer-internal' and caring for result of FUNCALL"
  (equal-buffer-internal funcall input exp-output nil exp-return interactive))

(defun equal-buffer (funcall input exp-output &optional interactive)
  "Call `equal-buffer-internal' not caring for result of FUNCALL"
  (equal-buffer-internal funcall input exp-output t nil interactive))

;; ****************************************************************************
;; Explainers

(defun equal-buffer-internal-explain (funcall input exp-output ignore-return exp-return interactive)
  "Return an explanation why `equal-buffer-internal' failed with
these parameters"
  (let ((test-result (run-test input funcall interactive))
	(exp-buf (and exp-output (string2buf exp-output))))
    (destructuring-bind (ok-string ok-point ok-return)
	(compare-test test-result exp-output ignore-return exp-return)
      (let (result)
	(if (not ok-return)
	    (push (list 'different-return-values
			(ert--explain-not-equal (car test-result) exp-return))
		  result))
	(if (not ok-point)
	    (push (list 'different-points
			(buf-string (cdr test-result))
			(buf-string exp-buf))
		  result))
	(if (not ok-string)
	    (push (list 'different-buffer-contents
			(ert--explain-not-equal
			 (buf-content (cdr test-result)) (buf-content exp-buf)))
		  result))
	result))))

(defun equal-buffer-return-explain (funcall input exp-output exp-return &optional interactive)
  (equal-buffer-internal-explain funcall input exp-output nil exp-return interactive))

(put 'equal-buffer-return 'ert-explainer 'equal-buffer-return-explain)

(defun equal-buffer-explain (funcall input exp-output &optional interactive)
  (equal-buffer-internal-explain funcall input exp-output t nil interactive))

(put 'equal-buffer 'ert-explainer 'equal-buffer-explain)
