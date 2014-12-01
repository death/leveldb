;;;; +----------------------------------------------------------------+
;;;; | LevelDB Common Lisp bindings                       DEATH, 2013 |
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
   (cache
    :initarg :cache
    :accessor db-cache)
   (handle
    :initarg :handle
    :accessor db-handle)
   (name
    :initarg :name
    :accessor db-name)
   (snapshots
    :initform (make-weak-hash-table :weakness :key)
    :accessor db-snapshots)))

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

(defmacro with-write-options ((var &key sync) &body forms)
  `(let ((,var (leveldb-writeoptions-create)))
     (unwind-protect
          (progn
            (leveldb-writeoptions-set-sync ,var ,sync)
            ,@forms)
       (leveldb-writeoptions-destroy ,var))))

(defmacro with-read-options ((var &key verify-checksums fill-cache snapshot) &body forms)
  `(let ((,var (leveldb-readoptions-create)))
     (unwind-protect
          (progn
            (leveldb-readoptions-set-verify-checksums ,var ,verify-checksums)
            (leveldb-readoptions-set-fill-cache ,var ,fill-cache)
            (when ,snapshot
              (leveldb-readoptions-set-snapshot ,var (snapshot-handle ,snapshot)))
            ,@forms)
       (leveldb-readoptions-destroy ,var))))

(defmacro with-octets-buffer ((var vector) &body forms)
  #+sbcl
  `(cffi:with-pointer-to-vector-data (,var ,vector)
     ,@forms)
  ;; Implementations like ACL/LW/CCL have special construction
  ;; requirements for "shareable vectors" that can be passed to
  ;; foreign code.  Since there's no predicate for distinguishing
  ;; those and ordinary octet vectors, and since we want to allow for
  ;; the latter, we just copy on those implementations.
  #-sbcl
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

(defun call-with-open-db (function name &rest options)
  (let ((db (apply #'open name options)))
    (unwind-protect
         (funcall function db)
      (close db))))

(defmacro with-open-db ((var name &rest options) &body forms)
  `(call-with-open-db (lambda (,var) ,@forms) ,name ,@options))

(defun open (name &key (if-does-not-exist :create)
                       (lru-cache-capacity nil))
  (let ((open-options (leveldb-options-create))
        (cache nil)
        (name (native-namestring name)))
    (leveldb-options-set-create-if-missing open-options (ecase if-does-not-exist (:error nil) (:create t)))
    (when lru-cache-capacity
      (setf cache (leveldb-cache-create-lru lru-cache-capacity))
      (leveldb-options-set-cache open-options cache))
    (with-errptr (errptr)
      (let ((handle (leveldb-open open-options name errptr)))
        (cond ((and (not (null-pointer-p handle))
                    (null-pointer-p (mem-ref errptr :pointer)))
               (make-instance 'db
                              :open-options open-options
                              :cache cache
                              :handle handle
                              :name name))
              (t (leveldb-options-destroy open-options)
                 (when cache
                   (leveldb-cache-destroy cache))
                 (check-errptr errptr)
                 (error 'leveldb-error :message "null db handle without error?")))))))

(defun close (db)
  (let ((handle (db-handle db))
        (snapshots (db-snapshots db)))
    (when handle
      (maphash (lambda (snapshot-handle whatever)
                 (declare (ignore whatever))
                 (remhash snapshot-handle snapshots)
                 (leveldb-release-snapshot handle snapshot-handle))
               snapshots)
      (leveldb-close handle)
      (leveldb-options-destroy (db-open-options db))
      (when (db-cache db)
        (leveldb-cache-destroy (db-cache db))
        (setf (db-cache db) nil))
      (setf (db-handle db) nil)
      (setf (db-open-options db) nil))))

(defun put (db key val &key sync)
  (with-octets-buffer (fkey key)
    (with-octets-buffer (fval val)
      (with-errptr (errptr)
        (with-write-options (options :sync sync)
          (leveldb-put (db-handle db) options
                       fkey (length key)
                       fval (length val)
                       errptr)
          (check-errptr errptr))))))

(defun puts (db key val &key sync)
  (put db (string-to-octets key) (string-to-octets val) :sync sync))

(defun delete (db key &key sync)
  (with-octets-buffer (fkey key)
    (with-errptr (errptr)
      (with-write-options (options :sync sync)
        (leveldb-delete (db-handle db) options
                        fkey (length key)
                        errptr)
        (check-errptr errptr)))))

(defun deletes (db key &key sync)
  (delete db (string-to-octets key) :sync sync))

(defun get (db key &key verify-checksums (fill-cache t) snapshot)
  (with-octets-buffer (fkey key)
    (with-errptr (errptr)
      (with-foreign-object (vallen 'size-t)
        (setf (mem-ref vallen 'size-t) 0)
        (with-read-options (options :verify-checksums verify-checksums
                                    :fill-cache fill-cache
                                    :snapshot snapshot)
          (let ((ret (leveldb-get (db-handle db) options
                                  fkey (length key)
                                  vallen errptr)))
            (check-errptr errptr)
            (foreign-octets-to-lisp ret vallen)))))))

(defun gets (db key &key verify-checksums (fill-cache t) snapshot)
  (let ((octets (get db (string-to-octets key)
                     :verify-checksums verify-checksums
                     :fill-cache fill-cache
                     :snapshot snapshot)))
    (when octets
      (octets-to-string octets))))

(defun destroy (name)
  (let ((options (leveldb-options-create))
        (name (native-namestring name)))
    (with-errptr (errptr)
      (leveldb-destroy-db options name errptr)
      (leveldb-options-destroy options)
      (check-errptr errptr))))

(defun repair (name)
  (let ((options (leveldb-options-create))
        (name (native-namestring name)))
    (with-errptr (errptr)
      (leveldb-repair-db options name errptr)
      (leveldb-options-destroy options)
      (check-errptr errptr))))

(defun call-with-iterator (db function &key (seek :first) verify-checksums (fill-cache t) snapshot)
  (with-read-options (options :verify-checksums verify-checksums
                              :fill-cache fill-cache
                              :snapshot snapshot)
    (let ((iter (leveldb-create-iterator (db-handle db) options)))
      (seek iter seek)
      (unwind-protect
           (funcall function iter)
        (leveldb-iter-destroy iter)))))

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

(defun map (db function &key (direction :forward) (seek :first)
                             (interest :both) (strings nil)
                             (limit nil)
                             verify-checksums (fill-cache t) snapshot)
  (when strings
    (setf function (pass-as-strings function))
    (when (stringp seek)
      (setf seek (string-to-octets seek))))
  (call-with-iterator db
                      (lambda (iter)
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
                      :seek seek
                      :verify-checksums verify-checksums
                      :fill-cache fill-cache
                      :snapshot snapshot))

(defun write (db batch &key sync)
  (let ((wb (leveldb-writebatch-create)))
    (unwind-protect
         (progn
           (dolist (action batch)
             (ecase (first action)
               (:put
                (destructuring-bind (key value) (rest action)
                  (with-octets-buffer (fkey key)
                    (with-octets-buffer (fvalue value)
                      (leveldb-writebatch-put wb fkey (length key) fvalue (length value))))))
               (:delete
                (destructuring-bind (key) (rest action)
                  (with-octets-buffer (fkey key)
                    (leveldb-writebatch-delete wb fkey (length key)))))))
           (with-errptr (errptr)
             (with-write-options (options :sync sync)
               (leveldb-write (db-handle db) options wb errptr)
               (check-errptr errptr))))
      (leveldb-writebatch-destroy wb))))

(defun property-value (db name)
  (let ((value (leveldb-property-value (db-handle db) name)))
    (unless (null-pointer-p value)
      (prog1 (foreign-string-to-lisp value)
        (leveldb-free value)))))

(defclass snapshot ()
  ((handle :initarg :handle :reader snapshot-handle)))

(defun snapshot (db)
  (let ((handle (leveldb-create-snapshot (db-handle db)))
        (snapshots (db-snapshots db)))
    (setf (gethash handle snapshots) t)
    (finalize (make-instance 'snapshot :handle handle)
              (lambda ()
                (when (gethash handle snapshots)
                  (remhash handle snapshots)
                  (leveldb-release-snapshot (db-handle db) handle))))))

(defun approximate-sizes (db ranges)
  ;; Works only with strings, for now...
  (let ((num-ranges (length ranges)))
    (with-foreign-objects ((range-start-key :pointer num-ranges)
                           (range-limit-key :pointer num-ranges)
                           (range-start-key-len 'size-t num-ranges)
                           (range-limit-key-len 'size-t num-ranges)
                           (sizes :uint64 num-ranges))
      (loop for i from 0
            for (start-key limit-key) in ranges
            do (multiple-value-bind (fstart-key fstart-key-len)
                   (foreign-string-alloc start-key)
                 (multiple-value-bind (flimit-key flimit-key-len)
                     (foreign-string-alloc limit-key)
                   (setf (mem-aref range-start-key :pointer i) fstart-key)
                   (setf (mem-aref range-start-key-len 'size-t i) fstart-key-len)
                   (setf (mem-aref range-limit-key :pointer i) flimit-key)
                   (setf (mem-aref range-limit-key-len 'size-t i) flimit-key-len))))
      (leveldb-approximate-sizes (db-handle db) num-ranges
                                 range-start-key range-start-key-len
                                 range-limit-key range-limit-key-len
                                 sizes)
      (loop for i from 0 below num-ranges
            do (foreign-string-free (mem-aref range-limit-key :pointer i))
            do (foreign-string-free (mem-aref range-start-key :pointer i))
            collect (mem-aref sizes :uint64 i)))))

;; options [comparator[compare name] filter-policy[create keymatch name, bloom]
;;          error-if-exists paranoid-checks compression env[default] info-log
;;          write-buffer-size max-open-files block-size block-restart-interval]
;; compact-range
