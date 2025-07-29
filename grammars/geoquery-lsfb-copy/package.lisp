(in-package :cl-user)

(defpackage :geoquery-lsfb-grammar-copy
  (:documentation "A package for the geoquery-lsfb grammar")
  (:use
   :common-lisp
   :utils
   :web-interface
   :monitors
   :irl
   :fcg
   :slp)
  (:import-from :cl-user
   *babel-corpora*)
  (:import-from :slp
   jsonl->list-of-json-alists
   replace-spaces
   make-fingerspelling
   trace-slp
   load-geoquery-corpus-jsonl
   slp::right-hand-articulation
   slp::left-hand-articulation
   slp::two-hand-articulation
   slp::during
   slp::end-coincides
   slp::start-coincides
   predicates))
