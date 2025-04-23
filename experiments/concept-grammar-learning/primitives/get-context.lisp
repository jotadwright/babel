;;;; get-context.lisp

(in-package :cgl)

;; -----------------------
;; GET-CONTEXT primtive ;;
;; -----------------------

;(export '(get-context))

(defprimitive get-context ((context clevr-object-set))
  ;; first case; consistency check
  ((context =>)
   (equal-entity (get-data ontology 'clevr-context) context))
  
  ;; second case; bind the context from the ontology
  ((=> context)
   (bind (context 1.0 (get-data ontology 'clevr-context))))
  :primitive-inventory *clevr-primitives*)

