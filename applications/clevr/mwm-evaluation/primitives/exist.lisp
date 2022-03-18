;;;; exist.lisp

(in-package :mwm-evaluation)

;; -----------------
;; EXIST primtive ;;
;; -----------------
;; Check whether there is an element in a set

;(export '(exist))

(defprimitive exist ((target-bool boolean-category)
                     (source-set mwm::mwm-object-set))
  ;; first case; give source-set, compute target-bool
  ((source-set => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (length> (objects source-set) 0)
             'yes 'no))))
     (bind (target-bool 1.0 boolean-category))))

  ;; second case; given source-set and target-bool, check consistency
  ((source-set target-bool =>)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (length> (objects source-set) 0)
             'yes 'no))))
     (equal-entity target-bool boolean-category)))
  :primitive-inventory *mwm-primitives*)