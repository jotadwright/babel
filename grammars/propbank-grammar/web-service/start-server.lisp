
(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(defparameter *restored-grammar*
  (cl-store:restore
   #+sbcl (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                          :name "propbank-grammar-ontonotes-ewt-core-roles-sbcl"
                          :type "fcg")
   #+lispworks (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                        :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                        :type "fcg")))

(export '(*restored-grammar*))

(fcg-server::start-fcg-server :address "127.0.0.1"
                  :port 1170
                  :grammar-systems '(:propbank-grammar))

;; curl 127.0.0.1:1170/comprehend-and-extract-frames -H "Content-Type: application/json" -d '{"utterance":"He told him a story", "package": "propbank-grammar", "grammar": "*restored-grammar*", "timeout": 100}'
