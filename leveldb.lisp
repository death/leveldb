;;;; +----------------------------------------------------------------+
;;;; | LevelDB Lisp bindings                              DEATH, 2013 |
;;;; +----------------------------------------------------------------+

(in-package #:leveldb)

(defun version ()
  (values (leveldb-major-version) (leveldb-minor-version)))

(define-condition leveldb-error (error)
  ((message :initarg :message
            :reader leveldb-error-message))
  (:report (lambda (c s)
             (format s "LevelDB error: ~S"
                     (leveldb-error-message c)))))

(defclass db ()
  ((open-options
    :initarg :open-options
    :accessor db-open-options)
   (write-options
    :initarg :write-options
    :accessor db-write-options)
   (read-options
    :initarg :read-options
    :accessor db-read-options)
   (handle
    :initarg :handle
    :accessor db-handle)
   (name
    :initarg :name
    :accessor db-name)))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~S ~:[(closed)~;(opened)~]" (db-name db) (db-handle db)))
  db)

(defmacro with-errptr ((var) &body forms)
  `(with-foreign-object (,var :pointer)
     (setf (mem-ref ,var :pointer) (null-pointer))
     ,@forms))

(defun check-errptr (errptr)
  (let ((err (mem-ref errptr :pointer)))
    (when (not (null-pointer-p err))
      (let ((s (foreign-string-to-lisp err)))
        (leveldb-free err)
        (error 'leveldb-error :message s)))))

(defmacro with-octets-buffer ((var vector) &body forms)
  (let ((gvector (gensym))
        (glen (gensym))
        (gi (gensym)))
    `(let* ((,gvector ,vector)
            (,glen (length ,gvector)))
       (with-foreign-object (,var :unsigned-char ,glen)
         (dotimes (,gi ,glen)
           (setf (mem-aref ,var :unsigned-char ,gi)
                 (aref ,gvector ,gi)))
         ,@forms))))

(defun foreign-octets-to-lisp (ptr vallen &optional (free t))
  (unless (null-pointer-p ptr)
    (let* ((n (mem-ref vallen 'size-t))
           (val (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n)
        (setf (aref val i)
              (mem-aref ptr :unsigned-char i)))
      (when free
        (leveldb-free ptr))
      val)))

(defun call-with-open-db (function name &key (if-does-not-exist :create))
  (let ((db (open name :if-does-not-exist if-does-not-exist)))
    (unwind-protect
         (funcall function db)
      (close db))))

(defmacro with-open-db ((var name &rest options) &body forms)
  `(call-with-open-db (lambda (,var) ,@forms) ,name ,@options))

(defun open (name &key (if-does-not-exist :create))
  (let ((open-options (leveldb-options-create))
        (write-options (leveldb-writeoptions-create))
        (read-options (leveldb-readoptions-create)))
    (leveldb-options-set-create-if-missing open-options (ecase if-does-not-exist (:error nil) (:create t)))
    (with-errptr (errptr)
      (let ((handle (leveldb-open open-options name errptr)))
        (cond ((and (not (null-pointer-p handle))
                    (null-pointer-p (mem-ref errptr :pointer)))
               (make-instance 'db
                              :open-options open-options
                              :write-options write-options
                              :read-options read-options
                              :handle handle
                              :name name))
              (t (leveldb-readoptions-destroy read-options)
                 (leveldb-writeoptions-destroy write-options)
                 (leveldb-options-destroy open-options)
                 (check-errptr errptr)
                 (error 'leveldb-error :message "null db handle without error?")))))))

(defun close (db)
  (let ((handle (db-handle db)))
    (when handle
      (leveldb-close handle)
      (leveldb-readoptions-destroy (db-read-options db))
      (leveldb-writeoptions-destroy (db-write-options db))
      (leveldb-options-destroy (db-open-options db))
      (setf (db-handle db) nil)
      (setf (db-read-options db) nil)
      (setf (db-write-options db) nil)
      (setf (db-open-options db) nil))))

(defun put (db key val)
  (with-octets-buffer (fkey key)
    (with-octets-buffer (fval val)
      (with-errptr (errptr)
        (leveldb-put (db-handle db) (db-write-options db)
                     fkey (length key)
                     fval (length val)
                     errptr)
        (check-errptr errptr)))))

(defun puts (db key val)
  (put db (string-to-octets key) (string-to-octets val)))

(defun delete (db key)
  (with-octets-buffer (fkey key)
    (with-errptr (errptr)
      (leveldb-delete (db-handle db) (db-write-options db)
                      fkey (length key)
                      errptr)
      (check-errptr errptr))))

(defun deletes (db key)
  (delete db (string-to-octets key)))

(defun get (db key)
  (with-octets-buffer (fkey key)
    (with-errptr (errptr)
      (with-foreign-object (vallen 'size-t)
        (setf (mem-ref vallen 'size-t) 0)
        (let ((ret (leveldb-get (db-handle db) (db-read-options db)
                                fkey (length key)
                                vallen errptr)))
          (check-errptr errptr)
          (foreign-octets-to-lisp ret vallen))))))

(defun gets (db key)
  (octets-to-string (get db (string-to-octets key))))

(defun destroy (name)
  (let ((options (leveldb-options-create)))
    (with-errptr (errptr)
      (leveldb-destroy-db options name errptr)
      (leveldb-options-destroy options)
      (check-errptr errptr))))

(defun repair (name)
  (let ((options (leveldb-options-create)))
    (with-errptr (errptr)
      (leveldb-repair-db options name errptr)
      (leveldb-options-destroy options)
      (check-errptr errptr))))

(defun call-with-iterator (function db &key (seek :first))
  (let ((iter (leveldb-create-iterator (db-handle db) (db-read-options db))))
    (seek iter seek)
    (unwind-protect
         (funcall function iter)
      (leveldb-iter-destroy iter))))

(defun seek (iter where)
  (case where
    (:first
     (leveldb-iter-seek-to-first iter))
    (:last
     (leveldb-iter-seek-to-last iter))
    (t
     (with-octets-buffer (fkey where)
       (leveldb-iter-seek iter fkey (length where))))))

(defun valid-p (iter)
  (leveldb-iter-valid iter))

(defun next (iter)
  (leveldb-iter-next iter))

(defun prev (iter)
  (leveldb-iter-prev iter))

(defun key (iter)
  (with-foreign-object (keylen 'size-t)
    (foreign-octets-to-lisp (leveldb-iter-key iter keylen) keylen nil)))

(defun value (iter)
  (with-foreign-object (vallen 'size-t)
    (foreign-octets-to-lisp (leveldb-iter-value iter vallen) vallen nil)))

(defun pass-as-strings (function)
  (lambda (&rest octet-vectors)
    (apply function (mapcar #'octets-to-string octet-vectors))))

(defun map (function db &key (direction :forward) (seek :first)
                             (interest :both) (strings nil)
                             (limit nil))
  (when strings
    (setf function (pass-as-strings function))
    (when (stringp seek)
      (setf seek (string-to-octets seek))))
  (call-with-iterator (lambda (iter)
                        (loop for n from 0
                              while (valid-p iter)
                              do (when (and limit (= n limit))
                                   (return))
                                 (ecase interest
                                   (:keys
                                    (funcall function (key iter)))
                                   (:values
                                    (funcall function (value iter)))
                                   (:both
                                    (funcall function (key iter) (value iter))))
                                 (ecase direction
                                   (:forward
                                    (next iter))
                                   (:backward
                                    (prev iter)))))
                      db
                      :seek seek))

;; options [comparator[compare name] filter-policy[create keymatch name, bloom]
;;          create-if-missing error-if-exists paranoid-checks compression env[default] info-log
;;          write-buffer-size max-open-files cache[lru] block-size block-restart-interval]
;; read-options [verify-checksums fill-cache snapshot]
;; write-options [sync]
;; write [batch: clear, put, delete, iterate]
;; snapshot [create release]
;; property-value
;; approximate-sizes
;; compact-range
