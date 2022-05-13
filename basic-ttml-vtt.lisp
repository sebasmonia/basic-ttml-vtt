;;;; basic-ttml-vtt.lisp

(in-package #:basic-ttml-vtt)

(defun test  ()
  (convert "Captions_en-US.ttml"))

(defun convert (input-ttml &optional output-vtt)
  "Convert INPUT-TTML to OUTPUT-VTT.
If the output filename is not provided, only the extension is changed."
  ;; reading the whole file is not perfect but I don't expect a subs file to cause an OOM...
  (let* ((ttml-root (plump:parse (uiop:read-file-string input-ttml)))
         (regions-ht (parse-regions ttml-root))
         (p-tags (parse-paragraphs ttml-root regions-ht)))
    (
                       ))

(defun parse-regions (ttml-root)
  "Extract the data from TTML-ROOT into a hashtable of `region' instances.
The keys are the regions xml:id attribute."
  (let ((regions-ht (make-hash-table :test 'equal)))
    (loop for region-node in (plump:get-elements-by-tag-name ttml-root "region")
          for region = (make-region-from-node region-node)
          for name = (xml-id region)
          do (setf (gethash name regions-ht) region)
          finally (return regions-ht))))

(defun parse-paragraphs (ttml-root regions-ht)
  "Extract the data from TTML-ROOT into a vector of `paragraph' instances.
REGIONS-HT is used to assign the correct `region' on each instance. "
  ;; in this case order matters, so we'll use child-elements instead of getting
  ;; all elements by tag (which is unordered, although testing shows it is
  ;; inverse order, i assume i can't rely on that)
  (loop for paragraph-node across (plump:child-elements
                                   ;; TODO: my particular inputs have only one div, that is not
                                   ;; always the case
                                   (first (plump:get-elements-by-tag-name ttml-root "div")))
        for referenced-region = (gethash (plump:get-attribute paragraph-node "region") regions-ht)
        collect (make-paragraph-from-node paragraph-node referenced-region)))

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
