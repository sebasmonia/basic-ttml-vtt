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
  (format nil "~a --> ~a ~a ~a ~%~a~%~%"
          (adjust-time+frame-to-millis (car (begin-end entity))
                                       (framerate entity)
                                       (framerate-multiplier entity))
          (adjust-time+frame-to-millis (cdr (begin-end entity))
                                       (framerate entity)
                                       (framerate-multiplier entity))
          (to-vtt-format (style entity))
          (to-vtt-format (region entity))
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

(defmethod to-vtt-format ((entity style))
  "Convert to VTT a style ENTITY. As of now only the tts:textAlign is converted."
  (let ((text-align (text-align entity)))
    (format nil "align:~a"
            (cond ((member text-align '("start" "left") :test #'string-equal) "start")
                  ((member text-align '("center" "justify") :test #'string-equal) "middle")
                  ((member text-align '("end" "right") :test #'string-equal) "end")))))

(defmethod to-vtt-format ((entity region))
  "Convert to VTT a region ENTITY, by mapping the coordinates of the origin+extent.
How they are converted depends on the tts:displayAlign property in the source."
  ;; TODO: only looking for before/after rather than the whole range of possible values for
  ;; this property. A full conversion has to handle more values.
  (let ((y-coordinate (cdr (origin entity)))
        (y-region-length (cdr (extent entity))))
    (format nil "position:~a% line:~a%"
            ;; position in the middle of the screen (horizontal alignment
            ;; will come form the style)
            50
            ;; vertical alignment is where it gets interesting...
            (if (string-equal "before" (display-align entity))
                ;; and keep the vertical line position at the top
                y-coordinate
                ;; but if it's "after", then we need to get the bottom of the rectangle
                (+ y-coordinate y-region-length)))))
