;;;; ttml-entities.lisp

(in-package #:basic-ttml-vtt)

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
      (split-string (remove "%" input-string :test #'string-equal))
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

(defun make-paragraph-from-node (p-node region)
  "Create a `paragraph' instance from P-NODE, associated to REGION."
  (let ((style (plump:get-attribute p-node "style"))
        (begin (plump:get-attribute p-node "begin"))
        (end (plump:get-attribute p-node "end"))
        (p-texts (extract-p-texts p-node)))
    ;; TODO: begin-end should be adjusted via framerate + framerateMultiplier
    ;; rather than copied literally
    (make-instance 'paragraph
                   :style style
                   :begin-end (cons begin end)
                   :region region
                   :p-texts p-texts)))

(defun extract-p-texts (p-node)
  "Convert text in P-NODE to a list of pairs as expected in a `paragraph' p-texts property."
  (loop for child across (plump:children p-node)
        for tag = (tag-name-safe child)
        for text = (if (string-equal tag "br")
                       "#\Newline"
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
