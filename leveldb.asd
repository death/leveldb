;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                       DEATH, 2013 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:leveldb
  :description "LevelDB bindings for Common Lisp."
  :author "death <github.com/death>"
  :license "BSD"
  :depends-on (#:cffi #:babel)
  :serial t
  :components
  ((:file "packages")
   (:file "low-level")
   (:file "leveldb")))
