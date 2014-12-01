;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                                   |
;;;; +----------------------------------------------------------------+

(in-package #:leveldb)

;;; LevelDB library

(define-foreign-library libleveldb
  (t (:default "libleveldb")))

(use-foreign-library libleveldb)

;;; Other types

(defctype bool (:boolean :unsigned-char))

;;; DB operations

(defcfun leveldb-open :pointer
  (options :pointer)
  (name :string)
  (errptr :pointer))

(defcfun leveldb-close :void
  (db :pointer))

(defcfun leveldb-put :void
  (db :pointer)
  (options :pointer)
  (key :pointer)
  (keylen size-t)
  (val :pointer)
  (vallen size-t)
  (errptr :pointer))

(defcfun leveldb-delete :void
  (db :pointer)
  (options :pointer)
  (key :pointer)
  (keylen size-t)
  (errptr :pointer))

(defcfun leveldb-write :void
  (db :pointer)
  (options :pointer)
  (batch :pointer)
  (errptr :pointer))

(defcfun leveldb-get :pointer
  (db :pointer)
  (options :pointer)
  (key :pointer)
  (keylen size-t)
  (vallen :pointer)
  (errptr :pointer))

(defcfun leveldb-create-iterator :pointer
  (db :pointer)
  (options :pointer))

(defcfun leveldb-create-snapshot :pointer
  (db :pointer))

(defcfun leveldb-release-snapshot :void
  (db :pointer)
  (snapshot :pointer))

(defcfun leveldb-property-value :pointer
  (db :pointer)
  (propname :string))

(defcfun leveldb-approximate-sizes :void
  (db :pointer)
  (num-ranges :int)
  (range-start-key :pointer)
  (range-start-key-len :pointer)
  (range-limit-key :pointer)
  (range-limit-key-len :pointer)
  (sizes :pointer))

(defcfun leveldb-compact-range :void
  (db :pointer)
  (start-key :pointer)
  (start-key-len size-t)
  (limit-key :pointer)
  (limit-key-len size-t))

;;; Management operations

(defcfun leveldb-destroy-db :void
  (options :pointer)
  (name :string)
  (errptr :pointer))

(defcfun leveldb-repair-db :void
  (options :pointer)
  (name :string)
  (errptr :pointer))

;;; Iterator

(defcfun leveldb-iter-destroy :void
  (iter :pointer))

(defcfun leveldb-iter-valid bool
  (iter :pointer))

(defcfun leveldb-iter-seek-to-first :void
  (iter :pointer))

(defcfun leveldb-iter-seek-to-last :void
  (iter :pointer))

(defcfun leveldb-iter-seek :void
  (iter :pointer)
  (key :pointer)
  (keylen size-t))

(defcfun leveldb-iter-next :void
  (iter :pointer))

(defcfun leveldb-iter-prev :void
  (iter :pointer))

(defcfun leveldb-iter-key :pointer
  (iter :pointer)
  (keylen :pointer))

(defcfun leveldb-iter-value :pointer
  (iter :pointer)
  (vallen :pointer))

(defcfun leveldb-iter-get-error :void
  (iter :pointer)
  (errptr :pointer))

;;; Write batch

(defcfun leveldb-writebatch-create :pointer)

(defcfun leveldb-writebatch-destroy :void
  (wb :pointer))

(defcfun leveldb-writebatch-clear :void
  (wb :pointer))

(defcfun leveldb-writebatch-put :void
  (wb :pointer)
  (key :pointer)
  (keylen size-t)
  (val :pointer)
  (vallen size-t))

(defcfun leveldb-writebatch-delete :void
  (wb :pointer)
  (key :pointer)
  (keylen size-t))

(defcfun leveldb-writebatch-iterate :void
  (wb :pointer)
  (state :pointer)
  (put :pointer)
  (deleted :pointer))

;;; Options

(defcfun leveldb-options-create :pointer)

(defcfun leveldb-options-destroy :void
  (options :pointer))

(defcfun leveldb-options-set-comparator :void
  (options :pointer)
  (comparator :pointer))

(defcfun leveldb-options-set-filter-policy :void
  (options :pointer)
  (filter-policy :pointer))

(defcfun leveldb-options-set-create-if-missing :void
  (options :pointer)
  (value bool))

(defcfun leveldb-options-set-error-if-exists :void
  (options :pointer)
  (value bool))

(defcfun leveldb-options-set-paranoid-checks :void
  (options :pointer)
  (value bool))

(defcfun leveldb-options-set-env :void
  (options :pointer)
  (env :pointer))

(defcfun leveldb-options-set-info-log :void
  (options :pointer)
  (info-log :pointer))

(defcfun leveldb-options-set-write-buffer-size :void
  (options :pointer)
  (size size-t))

(defcfun leveldb-options-set-max-open-files :void
  (options :pointer)
  (max :int))

(defcfun leveldb-options-set-cache :void
  (options :pointer)
  (cache :pointer))

(defcfun leveldb-options-set-block-size :void
  (options :pointer)
  (size size-t))

(defcfun leveldb-options-set-block-restart-interval :void
  (options :pointer)
  (interval :int))

(defcenum compression
  :no-compression
  :snappy-compression)

(defcfun leveldb-options-set-compression :void
  (options :pointer)
  (compression compression))

;;; Comparator

(defcfun leveldb-comparator-create :pointer
  (state :pointer)
  (dtor :pointer)
  (compare :pointer)
  (name :pointer))

(defcfun leveldb-comparator-destroy :void
  (comparator :pointer))

;;; Filter policy

(defcfun leveldb-filterpolicy-create :pointer
  (state :pointer)
  (dtor :pointer)
  (create-filter :pointer)
  (key-may-match :pointer)
  (name :pointer))

(defcfun leveldb-filterpolicy-destroy :void
  (filter-policy :pointer))

(defcfun leveldb-filterpolicy-create-bloom :pointer
  (bits-per-key :int))

;;; Read options

(defcfun leveldb-readoptions-create :pointer)

(defcfun leveldb-readoptions-destroy :void
  (options :pointer))

(defcfun leveldb-readoptions-set-verify-checksums :void
  (options :pointer)
  (value bool))

(defcfun leveldb-readoptions-set-fill-cache :void
  (options :pointer)
  (value bool))

(defcfun leveldb-readoptions-set-snapshot :void
  (options :pointer)
  (snapshot :pointer))

;;; Write options

(defcfun leveldb-writeoptions-create :pointer)

(defcfun leveldb-writeoptions-destroy :void
  (options :pointer))

(defcfun leveldb-writeoptions-set-sync :void
  (options :pointer)
  (value bool))

;;; Cache

(defcfun leveldb-cache-create-lru :pointer
  (capacity size-t))

(defcfun leveldb-cache-destroy :void
  (cache :pointer))

;;; Env

(defcfun leveldb-create-default-env :pointer)

(defcfun leveldb-env-destroy :void
  (env :pointer))

;;; Utility

(defcfun leveldb-free :void
  (ptr :pointer))

(defcfun leveldb-major-version :int)

(defcfun leveldb-minor-version :int)
