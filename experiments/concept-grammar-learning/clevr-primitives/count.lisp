;;;; count.lisp

(in-package :clg)

;; -----------------
;; COUNT primitive ;;
;; -----------------

;(export '(count!))

;; For the moment, this is implemented without using the integer
;; categories in the ontology. Let's see if it works like this.
(defprimitive count! ((target-num number)
                      (source-set clevr-object-set))
  ;; first case; given source-set, compute target
  ((source-set => target-num)
   (bind (target-num 1.0 (length (objects source-set)))))

  ((target-num => source-set)
   (let ((context (objects (get-data ontology 'clevr-context)))
         (all-combinations (combinations-of-length target-num)))
     (loop for objects in all-combinations
           for clevr-set = (make-instance 'clevr-object-set
                                          :objects objects
                                          :similarities nil)
           do (bind (source-set 1.0 clevr-set)))))

  ;; second case; given source and target, check consistency
  ((source-set target-num =>)
   (= target-num (length (objects source-set))))
  :primitive-inventory *clevr-primitives*)

