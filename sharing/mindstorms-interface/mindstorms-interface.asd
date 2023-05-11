(in-package :asdf)

(defsystem :mindstorms-interface
  :depends-on (:test-framework
               :utils
               :cl-json
               :drakma
               :assoc-utils)
  :serial t
  :components 
  ((:file "package")
   (:file "mindstorms")))
