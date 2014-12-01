;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                       DEATH, 2013 |
;;;; +----------------------------------------------------------------+

(in-package #:cl-user)

(defpackage #:leveldb
  (:use #:cl)
  (:import-from #:cffi #:define-foreign-library #:use-foreign-library
                #:defctype #:defcfun #:defcenum #:with-foreign-object
                #:mem-ref #:null-pointer-p #:foreign-string-to-lisp
                #:null-pointer #:mem-aref #:foreign-string-alloc
                #:foreign-string-free #:with-foreign-objects)
  (:import-from #:cffi-sys #:native-namestring)
  (:import-from #:babel #:string-to-octets #:octets-to-string)
  (:import-from #:trivial-garbage #:make-weak-hash-table #:finalize)
  (:shadow #:open #:close #:get #:delete #:map #:write)
  (:export #:version #:open #:close #:with-open-db
           #:put #:puts #:get #:gets #:delete #:deletes #:write
           #:map #:property-value #:snapshot #:approximate-sizes
           #:destroy #:repair))
