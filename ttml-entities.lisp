;;;; ttml-entities.lisp

(in-package #:basic-ttml-vtt)

(defclass ttml-subs-file ()
  ((path
    :initform nil
    :initarg :path
    :accessor path
    :documentation "The path of the TTML source file for the instance.")
   (framerate
    :initform 0
    :initarg :framerate
    :accessor framerate
    :documentation "The frameRate declared in the tt tag.")
   (drop-mode
    :initform nil
    :initarg :drop-mode
    :accessor drop-mode
    :documentation "The dropMode declared in the tt tag.")
   (framerate-multiplier
    :initform 0
    :initarg :framerate-multiplier
    :accessor framerate-multiplier
    :documentation "The frameRateMultiplier declared in the tt tag.")
   (regions
    :initform nil
    :initarg :regions
    :accessor regions
    :documentation "A hashtable with all the regions in the document, using their xml:id as key.")
   (paragraphs
    :initform nil
    :initarg :paragraphs
    :accessor paragraphs
    :documentation "A list with all the paragraphs in the document, in the order in which they appear."))
  (:documentation "Holds the data parsed out from a TTML document."))


(defun make-ttml-subs-file (path)
  "Create a `ttml-subs-file' from the file in PATH."
  (let* ((ttml-root (plump:parse (uiop:read-file-string "Captions_en-US.ttml")))
         (tt-tag (first (plump:get-elements-by-tag-name ttml-root "tt")))
         (regions-ht (parse-regions ttml-root))
         (framerate (parse-integer (plump:get-attribute tt-tag "ttp:frameRate")))
         (framerate-multiplier-value (uiop:split-string (plump:get-attribute
                                                         tt-tag
                                                         "ttp:frameRateMultiplier")))
         (framerate-multiplier (destructuring-bind (num denom) framerate-multiplier-value
                                 (/ (parse-integer num) (parse-integer denom))))
         (drop-mode (plump:get-attribute tt-tag "ttp:dropMode")))
    (make-instance 'ttml-subs-file
                   :path path
                   :framerate framerate
                   :framerate-multiplier framerate-multiplier
                   :drop-mode drop-mode
                   :regions regions-ht
                   :paragraphs (parse-paragraphs ttml-root regions-ht
                                                 framerate
                                                 framerate-multiplier
                                                 drop-mode))))

(defun parse-regions (ttml-root)
  "Extract the data from TTML-ROOT into a hashtable of `region' instances.
The keys are the regions xml:id attribute."
  (let ((regions-ht (make-hash-table :test 'equal)))
    (loop for region-node in (plump:get-elements-by-tag-name ttml-root "region")
          for region = (make-region-from-node region-node)
          for name = (xml-id region)
          do (setf (gethash name regions-ht) region)
          finally (return regions-ht))))

(defun parse-paragraphs (ttml-root regions-ht framerate framerate-multiplier drop-mode)
  "Extract the data from TTML-ROOT into a vector of `paragraph' instances.
REGIONS-HT is used to assign the correct `region' on each instance.
FRAMERATE, FRAMERATE-MULTIPLIER and DROP-MODe are needed when converting the paragraph to other
formats."
  ;; in this case order matters, so we'll use child-elements instead of getting
  ;; all elements by tag (which is unordered, although testing shows it is
  ;; inverse order, I assume I can't rely on that)
  (loop for paragraph-node across (plump:child-elements
                                   ;; TODO: my particular inputs have only one div, that is not
                                   ;; always the case
                                   (first (plump:get-elements-by-tag-name ttml-root "div")))
        for referenced-region = (gethash (plump:get-attribute paragraph-node "region") regions-ht)
        collect (make-paragraph-from-node paragraph-node
                                          referenced-region
                                          framerate
                                          framerate-multiplier
                                          drop-mode)))

(defclass region ()
  ((xml-id
    :initform nil
    :initarg :xml-id
    :accessor xml-id
    :documentation "Region xml:id attribute.")
   (origin
    :initform nil
    :initarg :origin
    :accessor origin
    :documentation "Region origin, stored in a cons cell (width . height).")
   (extent
    :initform nil
    :initarg :extent
    :accessor extent
    :documentation "Region extent, stored in a cons cell (width . height) for the region area. "))
   (:documentation "Holds the data parsed out from a TTML region. As of the initial version of this code, it only has
the minimum properties for the base conversion."))

(defun make-region-from-node (region-node)
  "Create a `region' instance from a Plump-parsed REGION-NODE."
  (let ((xml-id (plump:get-attribute region-node "xml:id"))
        (origin-string (plump:get-attribute region-node "tts:origin"))
        (extent-string (plump:get-attribute region-node "tts:extent")))
    (make-instance 'region
                   :xml-id xml-id
                   :origin (pair-of-percent-strings-to-cons origin-string)
                   :extent (pair-of-percent-strings-to-cons extent-string))))

(defun pair-of-percent-strings-to-cons (input-string)
  "Translate a \"{number}% {number}%\" INPUT-STRING to cons cell."
  (destructuring-bind (width height)
      ;; TODO: maybe it makes sense to remove the % sign, but doesn't
      ;; seem like it. Might make sense if we need to convert them to
      ;; numbers down the line.
      ;; (split-string (remove "%" input-string :test #'string-equal))
      (split-string input-string)
    (cons width height)))

(defclass paragraph ()
  ((style
    :initform nil
    :initarg :style
    :accessor style
    :documentation "p tag syle, as a string (for the time being).")
   (region
    :initform nil
    :initarg :region
    :accessor region
    :documentation "The `region' that this paragraph is associated to.")
   (framerate
    :initform nil
    :initarg :framerate
    :accessor framerate
    :documentation "The frameRate declared in the `ttml-subs-file' this p tag belongs to.")
   (drop-mode
    :initform nil
    :initarg :drop-mode
    :accessor drop-mode
    :documentation "The dropMode declared in the `ttml-subs-file' this p tag belongs to.")
   (framerate-multiplier
    :initform nil
    :initarg :framerate-multiplier
    :accessor framerate-multiplier
    :documentation "The frameRateMultiplier declared in the `ttml-subs-file' this p tag belongs to.")
   (begin-end
    :initform nil
    :initarg :begin-end
    :accessor begin-end
    :documentation "A cons cell (begin . end) both as strings. The conversion ")
   ;; TODO: this is OK in my particularly limited case but really I should have yet another
   ;; structure here for span-with-font-style & then text
   (p-texts
    :initform ""
    :initarg :p-texts
    :accessor p-texts
    :documentation "Each line within <p></p>, in a list of cons (style . text). Style can be nil."))
  (:documentation "A <p> or paragraph tag. One of these maps to a sub to display in the source file."))

(defun make-paragraph-from-node (p-node region framerate framerate-multiplier drop-mode)
  "Create a `paragraph' instance from P-NODE, associated to REGION.
FRAMERATE, FRAMERATE-MULTIPLIER and DROP-MODe are needed when converting the paragraph to other
formats."
  (let ((style (plump:get-attribute p-node "style"))
        (begin (plump:get-attribute p-node "begin"))
        (end (plump:get-attribute p-node "end"))
        (p-texts (extract-p-texts p-node)))
    ;; TODO: begin-end should be adjusted via framerate + framerateMultiplier
    ;; rather than copied literally
    (make-instance 'paragraph
                   :style style
                   :framerate framerate
                   :framerate-multiplier framerate-multiplier
                   :drop-mode drop-mode
                   :begin-end (cons begin end)
                   :region region
                   :p-texts p-texts)))

(defun extract-p-texts (p-node)
  "Convert text in P-NODE to a list of pairs as expected in a `paragraph' p-texts property."
  (loop for child across (plump:children p-node)
        for tag = (tag-name-safe child)
        for text = (if (string-equal tag "br")
                       ;; see https://lispcookbook.github.io/cl-cookbook/strings.html#creating-strings
                       ;; writing "#\Newline" results in a string that says "Newline"
                       (make-string 1 :initial-element #\Newline)
                       (plump:text child))
        for style = (style-attrib-safe child)
        collect (cons style text)))

(defun tag-name-safe (node)
  "Return the tag name of NODE, or nil if it's a text node."
  (unless (plump:text-node-p node)
    (plump:tag-name node)))

(defun style-attrib-safe (node)
  "Return the \"style\" attribute of NODE, if present, or nil.
This handles text nodes correctly (not querying attributes)."
  (unless (plump:text-node-p node)
    (plump:get-attribute node "tts:fontStyle")))
