;;;; unique.lisp

(in-package :cgl)

;; ------------------
;; UNIQUE primtive ;;
;; ------------------

;(export '(unique))

(defprimitive unique ((target-object clevr-object)
                      (source-set clevr-object-set))
  ;; first case; given source set, compute target object
  ((source-set => target-object)
   (when (length= (objects source-set) 1)
     (bind (target-object 1.0 (first (objects source-set))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-object =>)
   (and (length= (objects source-set) 1)
        (equal-entity target-object (first (objects source-set)))))
  :primitive-inventory *clevr-primitives*)

