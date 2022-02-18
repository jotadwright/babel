;;;; clevr-web-service.asd

(in-package :asdf)

(defsystem #:clevr-web-service
  :description "Web service to access the CLEVR Grammar"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :irl
               :fcg
               :cl-json
               :snooze
               :clevr-world
               :clevr-primitives
               :clevr-grammar
               :clevr-evaluation)
  :serial t
  :components ((:file "package")
               (:file "json-encoders")
               (:file "web-service")))
