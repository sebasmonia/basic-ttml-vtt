;;;; ttml-entities.lisp

(in-package #:basic-ttml-vtt)


;; This will make more sense when (if?) I convert the cons cells and p-text list
;; to proper classes, in that case the conversion will more tidy.
(defgeneric to-vtt-format (entity)
  (:documentation "Generate a VTT string out of a given TTML ENTITY."))

(defmethod to-vtt-format ((entity ttml-subs-file))
  "Return a VTT file from a `paragraph' ENTITY."
  (format nil "~a --> ~a align:start line:~a position:~a~%~a"
          ;; TODO: reminder, the time conversions need adjustment
          (car (begin-end entity))
          (cdr (begin-end entity))
          ;; TODO: which TTML property maps to what in VTT depends on the
          ;; writingMode of the source file. I'm assuming lrtd (the default)
          ;; line is the x coordinate of the region's origin
          (car (origin (region entity)))
          ;; position is the y coordinate of the region's origin
          (cdr (origin (region entity)))
          (p-text-to-cue (p-texts entity))))

(defmethod to-vtt-format ((entity paragraph))
  "Return a VTT cue (as string) from a `paragraph' ENTITY."
  (format nil "~a --> ~a align:start line:~a position:~a~%~a"
          ;; TODO: reminder, the time conversions need adjustment
          (car (begin-end entity))
          (cdr (begin-end entity))
          ;; TODO: which TTML property maps to what in VTT depends on the
          ;; writingMode of the source file. I'm assuming lrtd (the default)
          ;; line is the x coordinate of the region's origin
          (car (origin (region entity)))
          ;; position is the y coordinate of the region's origin
          (cdr (origin (region entity)))
          (p-text-to-cue (p-texts entity))))

;; A good example of where I should be calling `to-vtt-string' specializing
;; in a new type (p-texts, in this case)
(defun p-text-to-cue (p-texts)
  "Convert the list P-TEXTS to single cue text."
  ;; TODO: My immediate use case is either all italics or no style, but this
  ;; conversion can be a lot more complicated
  (with-output-to-string (formatted)
    (loop for (style . text) in p-texts
          for template = (if (string-equal style "italic")
                             "<i>~a</i>"
                             "~a")
          do
             (format formatted template text))))