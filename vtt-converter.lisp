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
    (with-open-file (out output-pathname :direction :output :external-format '(:utf-8 )
                         ;; TODO: don't supersede in the final version :)
                         ;; it is very useful for testing, though
                                         :if-exists :supersede)
      (write-sequence (format nil "WEBVTT~%~%") out)
      (loop for p in (paragraphs entity)
            do
               (write-sequence (to-vtt-format p) out)))
    output-pathname))

(defmethod to-vtt-format ((entity paragraph))
  "Return a VTT cue (as string) from a `paragraph' ENTITY."
  (format nil "~a --> ~a align:start line:~a position:~a~%~a~%~%"
          (adjust-time+frame-to-millis (car (begin-end entity))
                                       (framerate entity)
                                       (framerate-multiplier entity))
          (adjust-time+frame-to-millis (cdr (begin-end entity))
                                       (framerate entity)
                                       (framerate-multiplier entity))
          ;; TODO: which TTML property maps to what in VTT depends on the
          ;; writingMode of the source file. I'm assuming lrtd (the default)
          ;; line is the x coordinate of the region's origin
          (car (origin (region entity)))
          ;; position is the y coordinate of the region's origin
          (cdr (origin (region entity)))
          (format nil "~{~a~}" (mapcar #'to-vtt-format (p-children entity)))))

(defun adjust-time+frame-to-millis (timecode framerate framerate-multiplier)
  "Adjust TIMECODE from timestamp:frames to timestamp.millis.
FRAMERATE and FRAMERATE-MULTIPLIER come from the source TTML file."
  ;; TODO: this conversion is good enough for the particular case I'm working on now, but a full
  ;; converter needs to account for a bigger variety of source TTML formats
  (destructuring-bind (hours minutes seconds frames) (uiop:split-string timecode :separator ":")
    (format nil "~a:~a:~a~1,3f"
            hours
            minutes
            seconds
            (/ (read-from-string frames) (* framerate framerate-multiplier)))))

(defmethod to-vtt-format ((entity p-tag-child))
  "Convert a single `p-tag-child' ENTITY to VTT text."
  ;; TODO: I am only considering italic becuase of the type of source content I'm testing with,
  ;; but in a full fledged converter, this is much more complex...
  (format nil (if (string-equal (tag-style entity) "italic")
                  "<i>~a</i>"
                  "~a")
          (tag-text entity)))
