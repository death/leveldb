;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                                   |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:leveldb
  :description "LevelDB bindings for Common Lisp."
  :author "death <github.com/death>"
  :license "BSD"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:babel #:trivial-garbage)
  :serial t
  :components
  ((:file "packages")
   (cffi-grovel:grovel-file "grovel")
   (:file "low-level")
   (:file "leveldb")))
