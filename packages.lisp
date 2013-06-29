;;;; +----------------------------------------------------------------+
;;;; | LevelDB Lisp bindings                              DEATH, 2013 |
;;;; +----------------------------------------------------------------+

(in-package #:cl-user)

(defpackage #:leveldb
  (:use #:cl)
  (:import-from #:cffi #:define-foreign-library #:use-foreign-library
                #:defctype #:defcfun #:defcenum)
  (:import-from #:babel #:string-to-octets #:octets-to-string)
  (:shadow #:open #:close #:get #:delete #:map #:write)
  (:export #:version #:open #:close #:with-open-db
           #:put #:puts #:get #:gets #:delete #:deletes #:write
           #:map #:property-value
           #:destroy #:repair))
