;;;; basic-ttml-vtt.asd

;; definition based on teh sample at
;; https://asdf.common-lisp.dev/asdf/The-defsystem-form.html

(defsystem #:basic-ttml-vtt
  :description "Basic conversion of TTML subtitles to VTT."
  :author "Sebastián Monía <code@sebasmonia.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "uiop" "plump")
  :components ((:file "package")
               (:file "basic-ttml-vtt")))
