;;;; count.lisp

(in-package :clevr-primitives)

;; -----------------
;; COUNT primtive ;;
;; -----------------

;(export '(count!))

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive count! ((target-num number)
                      (source-set clevr-object-set))
  ;; first case; given source-set, compute target
  ((source-set => target-num)
   (bind (target-num 1.0 (length (objects source-set)))))
  
  ;; second case; given source and target, check consistency
  ((source-set target-num =>)
   (= target-num (length (objects source-set)))))

