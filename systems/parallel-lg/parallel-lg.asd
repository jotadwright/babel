
(in-package :asdf)

(defsystem parallel-lg
  :author ""
  :maintainer ""
  :license ""
  :homepage ""
  :serial t
  :depends-on (utils fcg split-sequence bordeaux-threads)
  :components ((:file "package")
               (:module parallel-ng
                :serial t
                :components ((:file "experiment"))))
  :description "A babel package for running a language game experiment in parallel.")
