(in-package :visual-dialog)

;; -----------------
;; EXIST-OR-COUNT primitive 
;; -----------------

(defprimitive exist-or-count ((target entity)
                              (source-world-model world-model)
                              (memory world-model))
  ;; first case; give source-set, compute target-bool
  ((source-world-model memory => target)  
   (let ((history memory))
     (multiple-value-bind (last-set last-timestamp)
         (the-biggest #'timestamp (set-items history))
       (if (equal (question-type last-set) 'exist)
         (let ((boolean-category
                (find-entity-by-id
                 ontology
                 (if (> (get-length (object-set (first (set-items source-world-model)))) 0)
                   'yes 'no))))
           (bind (target 1.0 boolean-category))))
       (if (equal (question-type last-set) 'count-objects)
         (let ((count (length (objects (object-set (first (reverse (set-items source-world-model))))))))
           (bind (target 1.0 (find-entity-by-id (get-data ontology 'digits)
                                                (internal-symb (upcase (format nil "~r" count)))))))))))
:primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))
        

(defgeneric get-length (object)
  (:documentation "Gets the length of the thing"))

(defmethod get-length ((object object)) 1)
(defmethod get-length ((set world-model))
  (length (objects (object-set (first (set-items set))))))