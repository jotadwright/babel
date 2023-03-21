;;;; exist.lisp

(in-package :visual-dialog)

;; -----------------
;; EXIST primtive ;;
;; -----------------

(defprimitive exist ((target-bool boolean-category)
                     (source-world-model world-model))
  ;; first case; give source-set, compute target-bool
  ((source-world-model => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> (get-length source-world-model) 0)
             'yes 'no))))
     (bind (target-bool 1.0 boolean-category))))

  ;; second case; given source-set and target-bool, check consistency
  ((source-world-model target-bool =>)
   (if (set-items source-world-model)
     (let ((boolean-category
            (find-entity-by-id
             ontology
             (if (> (get-length source-world-model) 0)
               'yes 'no))))
       (equal-entity target-bool boolean-category))
     ))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))


(defgeneric get-length (object)
  (:documentation "Gets the length of the thing"))

(defmethod get-length ((object-set object-set))
  (length (objects object-set)))

(defmethod get-length ((set world-model))
  (length (collect-objects-from-world-model set)))
