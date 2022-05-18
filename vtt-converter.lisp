;;;; ttml-entities.lisp

(in-package #:basic-ttml-vtt)


;; This will make more sense when (if?) I convert the cons cells and p-text list
;; to proper classes, in that case the conversion will more tidy.
(defgeneric to-vtt-format (entity)
  (:documentation "Generate a VTT string out of a given TTML ENTITY."))

(defmethod to-vtt-format ((entity ttml-subs-file))
  "Write a VTT file out of TTML-SUBS-FILE, in the same location."
  (let* ((input-pathname (uiop:truename* (path entity)))
         (output-pathname (make-pathname :type "vtt" :defaults input-pathname)))
    (with-open-file (out output-pathname :direction :output)
      (loop for p in (paragraphs entity)
            do
               (write-sequence (to-vtt-format p) out)))))

;; TODO: need to link back the source file at all times...how...?
(defmethod to-vtt-format ((entity paragraph))
  "Return a VTT cue (as string) from a `paragraph' ENTITY."
  (format nil "~a --> ~a align:start line:~a position:~a~%~a"
          (adjust-time-frames-to-millis (car (begin-end entity))
                                        (framerate entity)
                                        (framerate-multiplier entity))
          (adjust-time-frames-to-millis (cdr (begin-end entity))
                                        (framerate entity)
                                        (framerate-multiplier entity))
          ;; TODO: which TTML property maps to what in VTT depends on the
          ;; writingMode of the source file. I'm assuming lrtd (the default)
          ;; line is the x coordinate of the region's origin
          (car (origin (region entity)))
          ;; position is the y coordinate of the region's origin
          (cdr (origin (region entity)))
          ;; CONTINUE FROM HERE
          ;; add an extra new line between cues
          (p-text-to-cue (format nil "~a~%" (p-texts entity)))))

(defun adjust-time-frames-to-millis (timecode framerate framerate-multiplier)
  "Adjust TIMECODE from timestamp:frames to timestamp.millis.
FRAMERATE and FRAMERATE-MULTIPLIER come from the source TTML file."
  ;; TODO: this conversion is good enough for the particular case I'm working on now, but a full
  ;; converter needs to account for a bigger variety of source TTML formats
  (destructuring-bind (hours minutes seconds frames) (uiop:split-string timecode :separator ":")
    (format nil "~a:~a:~a.~a"
            hours
            minutes
            seconds
            (floor (* (read-from-string frames) (* framerate framerate-multiplier))))))

    
;; a good example of where i should be calling `to-vtt-string' specializing
;; in a new type (p-texts, in this case)
(defun p-text-to-cue (p-texts)
  "convert the list p-texts to single cue text."
  ;; todo: my immediate use case is either all italics or no style, but this
  ;; conversion can be a lot more complicated
  (with-output-to-string (formatted)
    (loop for (style . text) in p-texts
          for template = (if (string-equal style "italic")
                             "<i>~a</i>~%"
                             "~a~%")
          do
             (format formatted template text))))
