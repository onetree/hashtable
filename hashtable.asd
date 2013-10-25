(in-package :asdf)

(defsystem :hashtable
  :version "0.0.1"
  :maintainer "Kazuki Nishikawa"
  :licence "MIT"
  :description "Generic hashtable implementation"
  :serial t
  :components
  ((:file "package")
   (:file "hashtable")))
