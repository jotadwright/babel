;;;; clevr-web-service.asd

(in-package :asdf)

(defsystem #:clevr-web-service
  :description "Web service to access the CLEVR Grammar"
  :author "Jens Nevens <jens@ai.vub.ac.be>"
  :license "GPL 3.0"
  :depends-on (:utils
               :irl
               :fcg
               :cl-json
               :snooze
               :clevr
               :clevr-grammar
               :clevr-evaluation)
  :serial t
  :components ((:file "package")
               (:file "json-encoders")
               (:file "web-service")))
