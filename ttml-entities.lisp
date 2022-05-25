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
   (styles
    :initform nil
    :initarg :styles
    :accessor styles
    :documentation "A hashtable with all the styles in the document, using their xml:id as key.")
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
  (let* ((ttml-root (plump:parse (uiop:read-file-string path)))
         (tt-tag (first (plump:get-elements-by-tag-name ttml-root "tt")))
         (styles-ht (parse-styles ttml-root))
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
                   :styles styles-ht
                   :regions regions-ht
                   :paragraphs (parse-paragraphs ttml-root
                                                 styles-ht
                                                 regions-ht
                                                 framerate
                                                 framerate-multiplier
                                                 drop-mode))))

(defun parse-styles (ttml-root)
  "Extract the data from TTML-ROOT into a hashtable of `style' instances.
The keys are the styles xml:id attribute."
  (let ((styles-ht (make-hash-table :test 'equal)))
    (loop for style-node in (plump:get-elements-by-tag-name ttml-root "style")
          for style = (make-style-from-node style-node)
          for name = (xml-id style)
          do (setf (gethash name styles-ht) style)
          finally (return styles-ht))))

(defun parse-regions (ttml-root)
  "Extract the data from TTML-ROOT into a hashtable of `region' instances.
The keys are the regions xml:id attribute."
  (let ((regions-ht (make-hash-table :test 'equal)))
    (loop for region-node in (plump:get-elements-by-tag-name ttml-root "region")
          for region = (make-region-from-node region-node)
          for name = (xml-id region)
          do (setf (gethash name regions-ht) region)
          finally (return regions-ht))))

(defun parse-paragraphs (ttml-root styles-ht regions-ht framerate framerate-multiplier drop-mode)
  "Extract the data from TTML-ROOT into a vector of `paragraph' instances.
STYLES-HT and REGIONS-HT are used to assign the correct `style' and `region' on each instance.
FRAMERATE, FRAMERATE-MULTIPLIER and DROP-MODe are needed when converting the paragraph to other
formats."
  ;; in this case order matters, so we'll use child-elements instead of getting
  ;; all elements by tag (which is unordered, although testing shows it is
  ;; inverse order, I assume I can't rely on that)
  (loop for paragraph-node across (plump:child-elements
                                   ;; TODO: my particular inputs have only one div, that is not
                                   ;; always the case
                                   (first (plump:get-elements-by-tag-name ttml-root "div")))
        for referenced-style = (gethash (plump:get-attribute paragraph-node "style") styles-ht)
        for referenced-region = (gethash (plump:get-attribute paragraph-node "region") regions-ht)
        collect (make-paragraph-from-node paragraph-node
                                          referenced-style
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
the minimum properties for a single use case of VTT conversion."))

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

(defclass style ()
  ((xml-id
    :initform nil
    :initarg :xml-id
    :accessor xml-id
    :documentation "Style xml:id attribute.")
   (text-align
    :initform nil
    :initarg :text-align
    :accessor text-align
    :documentation "Style text align, any of left, center, right, start, end or justify."))
  (:documentation "Holds the data parsed out from a TTML style node. As of the initial version of
this code, it only has the minimum properties for a single use case of VTT conversion."))

(defun make-style-from-node (style-node)
  "Create a `style' instance from a Plump-parsed STYLE-NODE."
  (make-instance 'style
                 :xml-id (plump:get-attribute style-node "xml:id")
                 :text-align (plump:get-attribute style-node "tts:textAlign")))

(defclass paragraph ()
  ((style
    :initform nil
    :initarg :style
    :accessor style
    :documentation "The `style' that this paragraph is associated to.")
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
   (p-children
    :initform nil
    :initarg :p-children
    :accessor p-children
    :documentation "Each line within <p></p>, in a list of `p-tag-child' objects."))
  (:documentation "A <p> or paragraph tag. One of these maps to a sub to display in the source file."))

(defun make-paragraph-from-node (p-node style region framerate framerate-multiplier drop-mode)
  "Create a `paragraph' instance from P-NODE, associated to STYLE and REGION.
FRAMERATE, FRAMERATE-MULTIPLIER and DROP-MODE are needed when converting the paragraph to other
formats."
  (let ((style (plump:get-attribute p-node "style"))
        (begin (plump:get-attribute p-node "begin"))
        (end (plump:get-attribute p-node "end"))
        (p-children (loop for child across (plump:children p-node)
                          collect (make-p-tag-child-from-plump-node child))))
    ;; TODO: begin-end should be adjusted via framerate + framerateMultiplier
    ;; rather than copied literally
    (make-instance 'paragraph
                   :style style
                   :framerate framerate
                   :framerate-multiplier framerate-multiplier
                   :drop-mode drop-mode
                   :begin-end (cons begin end)
                   :region region
                   :p-children p-children)))

(defclass p-tag-child ()
  ((tag-name
    :initform "text"
    :initarg :tag-name
    :accessor tag-name
    :documentation "The actual tag name. The default value means no sub-tag: an unformatted text node.")
   (tag-style
    :initform nil
    :initarg :tag-style
    :accessor tag-style
    :documentation "Text style for a <p> child tag/node. For the initial version only \"normal\" and \"italic\" are supported. nil is used if the property is not relevant or it is a text-only node.")
   (tag-text
    :initform ""
    :initarg :tag-text
    :accessor tag-text
    :documentation "The text contained in child tag/text node within a <p> tag."))
  (:documentation "Each child node (tag or text-only) within a <p> tag."))

(defun make-p-tag-child-from-plump-node (plump-node)
  "Create a `p-tag-child' from PLUMP-NODE, which must be a child of a <p> tag."
  (let* ((tag (if (plump:text-node-p plump-node)
                  "text"
                  (plump:tag-name plump-node)))
         (text (if (string-equal tag "br")
                   ;; see https://lispcookbook.github.io/cl-cookbook/strings.html#creating-strings
                   ;; writing "#\Newline" results in a string that says "Newline"
                   (make-string 1 :initial-element #\Newline)
                   (plump:text plump-node)))
         (style (unless (plump:text-node-p plump-node)
                  (plump:get-attribute plump-node "tts:fontStyle"))))
    (make-instance 'p-tag-child
                   :tag-style style
                   :tag-name tag
                   :tag-text text)))
