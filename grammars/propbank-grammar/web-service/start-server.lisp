
(ql:quickload :propbank-grammar)
(in-package :fcg)

;(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

(defparameter *restored-grammar*
  (cl-store:restore
   #+sbcl (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                          :name "propbank-grammar-ontonotes-ewt-core-roles-no-aux-cleaned-sbcl"
                          :type "fcg")
   #+lispworks (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                        :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                        :type "fcg")))

(export '(*restored-grammar*))

(fcg-server::start-fcg-server :address "127.0.0.1"
                  :port 1170
                  :grammar-systems '(:propbank-grammar))

;; curl 127.0.0.1:1170/extract-frames -H "Content-Type: application/json" -d '{"utterance":"He told him a story", "package": "propbank-grammar", "grammar": "*restored-grammar*", "timeout": 100}'
;;

#|
(defun run-test ()
(with-package :propbank-grammar
  ;(propbank-grammar::test propbank-grammar::*restored-grammar*)
  (propbank-grammar::comprehend-and-extract-frames "He told him a story" :cxn-inventory propbank-grammar::*restored-grammar*)
  ))

(fcg::run-test)
|#
