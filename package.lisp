;;;; package.lisp

(in-package #:cl)

(defpackage #:basic-ttml-vtt
            ;; I am not sold on this package nickname
            (:nicknames "btv" :btv)
            (:local-nicknames (:alex :alexandria))
            (:use #:common-lisp #:uiop)
            (:import-from :plump)
            (:export
             #:convert-to-vtt))

(in-package #:basic-ttml-vtt)
