;;;; ttml-entities.lisp

(in-package #:basic-ttml-vtt)

;; (defclass paragraph ()
;;   ((style
;;     :initform nil
;;     :initarg :style
;;     :accessor style
;;     :documentation "p tag syle, as a string (for the time being).")
;;    (region
;;     :initform nil
;;     :initarg :region
;;     :accessor region
;;     :documentation "The `region' that this paragraph is associated to.")
;;    (begin-end
;;     :initform nil
;;     :initarg :begin-end
;;     :accessor begin-end
;;     :documentation "A cons cell (begin . end) both as strings. The conversion ")
;;    ;; TODO: this is OK in my particularly limited case but really I should have yet another
;;    ;; structure here for span-with-font-style & then text
;;    (p-texts
;;     :initform ""
;;     :initarg :p-texts
;;     :accessor p-texts
;;     :documentation "Each line within <p></p>, in a list of cons (style . text). Style can be nil."))
;;   (:documentation "A <p> or paragraph tag. One of these maps to a sub to display in the source file."))


;; This will make more sense when (if?) I convert the cons cells and p-text list
;; to proper classes, in that case the conversion will more tidy.
(defgeneric to-vtt-string (entity)
  (:documentation "Generate a VTT string out of a given TTML ENTITY."))

(defmethod to-vtt-string ((entity paragraph))
  "Return a VTT cue (as string) from a `paragraph' ENTITY."
  (let ((cue-text "meh"))
`    (format t "~a --> ~a align:start line:~a position:~a~%~a"
            ;; TODO: reminder, the time conversions need adjustment
            (car (begin-end entity))
            (cdr (begin-end entity))
            ;; line is the x coordinate of the region's origin
            (car (origin (region entity)))
            ;; position is the y coordinate of the region's origin
            (cdr (origin (region entity)))
            ;; now to build the text
            cue-text)))
