
(in-package :asdf)

(defsystem :cl-jonathan
  :description "cl-jonathan provides the ease of use of cl-json, with the speed with jonathan"
  :depends-on (:cl-ppcre :utils :jonathan)
  :components 
  ((:file "package")
   (:file "encode")
   (:file "decode")))
