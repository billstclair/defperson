; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :defperson
  :description "Print name badges for ILC/ELS."
  :author "Unknown. Packaged by Bill St. Clair <bill@billstclair.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (:cl-pdf)
  :serial t
  :components
  ((:file "package")
   (:file "defperson")))
