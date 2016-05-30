;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                                   |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)
(defpackage leveldb-system
    (:use #:cl #:asdf))
(in-package :leveldb-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-system "cffi-grovel"))

(defsystem #:leveldb
  :description "LevelDB bindings for Common Lisp."
  :author "death <github.com/death>"
  :license "BSD"
  :depends-on (#:cffi #:babel #:trivial-garbage)
  :serial t
  :components
  ((:file "packages")
   (:cffi-grovel-file "grovel")
   (:file "low-level")
   (:file "leveldb")))
