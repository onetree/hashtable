(defpackage hashtable
  (:nicknames :hash)
  (:use :cl)
  (:shadow :cl
           :hash-table
           :make-hash-table
           :hash-table-p
           :hash-table-count
           :hash-table-rehash-size
           :hash-table-rehash-threshold
           :hash-table-size
           :hash-table-test
           :gethash
           :remhash
           :maphash
           :with-hash-table-iterator
           :clrhash)
  (:export
   :hash-code
   :hash-table
   :make-hash-table
   :hash-table-p
   :hash-table-count
   :hash-table-rehash-size
   :hash-table-rehash-threshold
   :hash-table-size
   :hash-table-test
   :gethash
   :remhash
   :maphash
   :with-hash-table-iterator
   :clrhash))
