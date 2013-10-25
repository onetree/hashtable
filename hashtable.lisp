(in-package :hashtable)

(defgeneric hash-code (object)
  (:documentation "Returns a hash code for `object'"))

(defmethod hash-code ((object t))
  (sxhash object))

(defstruct entry
  (hash 0 :type fixnum)
  key
  value)

(defstruct (hash-table (:conc-name ht-)
                       (:constructor %make-hash-table))
  ;; mutable fields
  (table nil :type array) ; the size of this array must be power of 2
  (size 0 :type fixnum)
  (threshold 0 :type fixnum)
  ;; immutable fields
  (rehash-size 0 :type single-float)
  (threshold-factor 0.0 :type single-float)
  (test nil :type function))

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":TEST ~A :COUNT ~A" (ht-test object) (ht-size object))))

(defparameter *default-initial-size* 16)
(defparameter *default-rehash-size* 2.0)
(defparameter *default-rehash-threshold* 0.75)

(defun calc-ideal-size (size)
  (loop with ret = 1
       while (< ret size)
       do (setf ret (ash ret 1))
       finally (return ret)))

(declaim (inline index-for))
(defun index-for (hash size)
  (declare (fixnum hash size))
  (logand hash (1- size)))

(defun make-hash-table (&key (test #'eql) size rehash-size rehash-threshold)
  (setf size (calc-ideal-size (or size *default-initial-size*))
        rehash-size (or rehash-size *default-rehash-size*)
        rehash-threshold (or rehash-threshold *default-rehash-threshold*))
  (%make-hash-table
   :table (make-array size :initial-element nil)
   :size 0
   :threshold (floor (* size rehash-threshold))
   :rehash-size rehash-size
   :threshold-factor rehash-threshold
   :test test))

(declaim (inline hash-table-count
                 hash-table-rehash-size
                 hash-table-rehash-threshold
                 hash-table-size
                 hash-table-test))
(defun hash-table-count (hash-table)
  (declare (hash-table hash-table))
  (ht-size hash-table))

(defun hash-table-rehash-size (hash-table)
  (declare (hash-table hash-table))
  (ht-rehash-size hash-table))

(defun hash-table-rehash-threshold (hash-table)
  (declare (hash-table hash-table))
  (ht-threshold-factor hash-table))

(defun hash-table-size (hash-table)
  (declare (hash-table hash-table))
  (array-total-size (ht-table hash-table)))

(defun hash-table-test (hash-table)
  (declare (hash-table hash-table))
  (ht-test hash-table))

(defun finder (hash key hash-table)
  (let ((test (ht-test hash-table)))
    (lambda (e)
      (and (= hash (entry-hash e))
           (funcall test key (entry-key e))))))

(defun gethash (key hash-table &optional default)
  (declare (hash-table hash-table))
  (let* ((hash (hash-code key))
         (idx (index-for hash (hash-table-size hash-table)))
         (entries (aref (ht-table hash-table) idx))
         (entry (find-if (finder hash key hash-table) entries)))
    (values (if entry (entry-value entry) default)
            (not (null entry)))))

(declaim (inline need-rehash-p))
(defun need-rehash-p (hash-table)
  (declare (hash-table hash-table))
  (< (ht-threshold hash-table) (ht-size hash-table)))

(defun rehash (hash-table)
  (declare (hash-table hash-table))
  (let* ((old-table (ht-table hash-table))
         (new-size (calc-ideal-size (floor (* (hash-table-size hash-table)
                                              (hash-table-rehash-size hash-table)))))
         (new-table (make-array new-size :initial-element nil))
         (new-threshold (floor (* new-size (hash-table-rehash-threshold hash-table)))))

    (setf (ht-table hash-table) new-table
          (ht-threshold hash-table) new-threshold)

    (loop for entries across old-table
       do (mapc (lambda (e)
                  (let ((i (index-for (entry-hash e) new-size)))
                    (push e (aref new-table i))))
                entries))))

(defun puthash (key hash-table value)
  (declare (hash-table hash-table))
  (let* ((hash (hash-code key))
         (idx (index-for hash (hash-table-size hash-table)))
         (entries (aref (ht-table hash-table) idx))
         (entry (find-if (finder hash key hash-table) entries)))
    (if entry
        (setf (entry-value entry) value)
        (progn
          (push (make-entry :hash hash :key key :value value) (aref (ht-table hash-table) idx))
          (incf (ht-size hash-table))
          (when (need-rehash-p hash-table)
            (rehash hash-table)))))
  value)

(defsetf gethash puthash)

(defun remhash (key hash-table)
  (declare (hash-table hash-table))
  (let* ((hash (hash-code key))
         (idx (index-for hash (hash-table-size hash-table)))
         (finder (finder hash key hash-table))
         (before-size (ht-size hash-table)))
    (loop for e in (aref (ht-table hash-table) idx)
       if (funcall finder e)
         do (decf (ht-size hash-table))
       else
         collect e into new-entries
       finally (setf (aref (ht-table hash-table) idx) new-entries))
    ;; Returns true if there was such an entry, or false otherwise
    (< (ht-size hash-table) before-size)))

(defun maphash (function hash-table)
  (declare (function function)
           (hash-table hash-table))
  (loop for entries across (ht-table hash-table)
     do (mapc (lambda (e)
                (funcall function (entry-key e) (entry-value e)))
              entries)))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((fun (make-symbol (format nil "~A-FUN" name))))
    `(let ((,fun (let* ((ht ,hash-table)
                        (size (hash-table-size ht))
                        (next-index 0)
                        (current-items nil))
                   (labels ((,name ()
                              (if current-items
                                  (pop current-items)
                                  (loop for i from next-index below size
                                     when (aref (ht-table ht) i)
                                     do (progn
                                          (setf next-index (1+ i)
                                                current-items (aref (ht-table ht) i))
                                          (let ((e (pop current-items)))
                                            (return-from ,name
                                              (values t (entry-key e) (entry-value e)))))))))
                     #',name))))
       (macrolet ((,name ()
                    '(funcall ,fun)))
         ,@body))))

(defun clrhash (hash-table)
  (declare (hash-table hash-table))
  (setf (ht-size hash-table) 0)
  (loop with table = (ht-table hash-table)
     for i from 0 below (array-total-size table)
     do (setf (aref table i) nil))
  hash-table)
