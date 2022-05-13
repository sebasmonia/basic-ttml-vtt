;;;; package.lisp

(in-package #:cl)

(defpackage #:basic-ttml-vtt
            (:local-nicknames (:alex :alexandria))
            (:use #:common-lisp #:uiop)
            (:import-from :plump)
            (:export
             #:convert
             )
            ;; I already dislike this nickname, and haven't even used it
            (:nicknames "btv" :btv))

(in-package #:basic-ttml-vtt)
