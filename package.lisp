;;;; package.lisp

(in-package #:cl)

(defpackage #:basic-ttml-vtt
            (:local-nicknames (:alex :alexandria))
            (:use #:common-lisp #:uiop)
            (:import-from :plump)
            (:export
             #:convert
             )
            ;; I am not sold on this package nickname
            (:nicknames "btv" :btv))

(in-package #:basic-ttml-vtt)
