; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; defperson package definition
;;;

(in-package :cl-user)

(cl:defpackage :defperson
  (:use :cl)
  (:export
   #:cards
   #:*people*
   ))
