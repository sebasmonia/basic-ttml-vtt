;;;; basic-ttml-vtt.lisp

(in-package #:basic-ttml-vtt)

(defun test  ()
  (convert "Captions_en-US.ttml"))

;; (defun convert (input-ttml &optional output-vtt)
;;   "Convert INPUT-TTML to OUTPUT-VTT.
;; If the output filename is not provided, only the extension is changed."
;;   ;; reading the whole file is not perfect but I don't expect a subs file to cause an OOM...
;;   (let* ((input-pathname (uiop:truename* input-ttml))
;;          (output-pathname (if output-vtt
;;                               (uiop:truename* output-vtt)
;;                               (make-pathname :type "vtt" :defaults input-pathname)))
;;          (source-ttml (make-ttml-subs-file input-ttml)))
;;     (write-vtt-file source-ttml)))


;;;; deflex.lisp -- Define "global lexical variables" in Common Lisp.
;;; Copyright (c) 2003-2007, 2011 Rob Warnock <rpw3@rpw3.org>.
(defmacro deflex (var val &optional (doc nil docp))
  "Define a top level (global) lexical VAR with initial value VAL,
  which is assigned unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the name |VAR| and the
  name *STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of
  kind 'VARIABLE. The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
	 (s1 (symbol-name var))
	 (s2 (symbol-name '#:*))
	 (s3 (symbol-package var))	; BUGFIX [see above]
	 (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
      `(progn
	 (defparameter ,backing-var ,val ,doc)
	 (setf (documentation ',var 'variable) ,doc)
	 (define-symbol-macro ,var ,backing-var))
      `(progn
	 (defparameter ,backing-var ,val)
	 (define-symbol-macro ,var ,backing-var)))))
