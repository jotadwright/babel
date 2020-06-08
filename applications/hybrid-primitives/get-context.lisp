;;;; get-context.lisp

(in-package :hybrid-primitives)

;; -----------------------
;; GET-CONTEXT primtive ;;
;; -----------------------

(defprimitive get-context ((context attention))
              )

(defprimitive get-context ((context clevr-object-set))
  ;; first case; consistency check
  ((context =>)
   (equal-entity (get-data ontology 'clevr-context) context))
  
  ;; second case; bind the context from the ontology
  ((=> context)
   (bind (context 1.0 (get-data ontology 'clevr-context)))))

