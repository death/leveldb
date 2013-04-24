;;;; +----------------------------------------------------------------+
;;;; | LevelDB Lisp bindings                              DEATH, 2013 |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:leveldb
  :description "LevelDB Lisp bindings."
  :depends-on (#:cffi)
  :serial t
  :components
  ((:file "packages")
   (:file "low-level")
   (:file "leveldb")))
