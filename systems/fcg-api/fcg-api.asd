(defsystem fcg-api
  :author ""
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license ""
  :homepage "https://gitlab.ai.vub.ac.be/ehai/fcg-api"
  :serial t
  :depends-on (:fcg :utils)
  :components ((:file "package")
               (:file "fcg-api"))
  :description "A high-level API to Fluid Construction Grammar")