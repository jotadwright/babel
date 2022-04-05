(in-package :visual-dialog)

;; -----------------
;; EXIST-OR-COUNT primitive 
;; -----------------

(defgeneric get-length (object)
  (:documentation "Gets the length of the thing"))

(defmethod get-length ((object object)) 1)
(defmethod get-length ((set world-model))
  (length (objects (object-set (first (set-items set))))))


(defprimitive exist-or-count ((target entity)
                              (source-set object-or-set))
  ;; first case; give source-set, compute target-bool
  ((source-set => target)  
   (let ((history (get-data ontology 'conversation-memory)))
     (multiple-value-bind (last-set last-timestamp)
         (the-biggest #'timestamp (set-items history))
       (if (equal (question-type last-set) 'exist)
         (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> (get-length source-set) 0)
             'yes 'no))))
           (bind (target 1.0 boolean-category))))
       (if (equal (question-type last-set) 'count-objects)
         (let ((count (length (objects (object-set (first (reverse (set-items source-set))))))))
     (bind (target 1.0 (find-entity-by-id (get-data ontology 'digits)
                                              (internal-symb (upcase (format nil "~r" count)))))))))))
  :primitive-inventory (*symbolic-primitives* *hybrid-primitives*))
         
         
   

