(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRL NODE HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-predicate-name ((irl-node irl::irl-program-processor-node))
  "Get the predicate name belonging to this node's primitive under evaluation."
  (first (irl::primitive-under-evaluation irl-node)))

(defmethod get-output-binding ((irl-node irl::irl-program-processor-node))
  "Get the binding belonging to the primary output of this node's primitive under evaluation."
  (let ((target-var (second (irl::primitive-under-evaluation irl-node)))
        (list-of-bindings (irl::bindings irl-node)))
    (find target-var list-of-bindings :key #'var)))  

(defmethod get-output-value ((irl-node irl::irl-program-processor-node))
  "Get the value belonging to the binding for the primary output of this node's primitive under evaluation."
  (let ((target-binding (get-output-binding irl-node)))
    (when target-binding
      (value target-binding))))

(defmethod get-output-kitchen-state ((irl-node irl::irl-program-processor-node))
  "Get the output kitchen state belonging to this node's primitive under evaluation."
  (let ((all-vars (irl::all-variables (list (irl::primitive-under-evaluation irl-node))))
        (list-of-bindings (irl::bindings irl-node)))
    (loop for var in all-vars
          for binding = (find var list-of-bindings :key #'var)
          if (eql (type-of (value binding)) 'kitchen-state)
            do (return (value binding)))))

(defmethod get-full-node-sequence ((irl-node irl::irl-program-processor-node))
  "Get the full sequence of nodes that led to the given IRL node.
   Sequence goes from the starting node with the first executed primitive up to and including the given IRL node."
  (let ((node irl-node)
        (node-seq '()))
    (loop do
         (push node node-seq)
         (setf node (parent node))
       while node)
    (rest node-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions Related to Kitchen Entity Locations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun similar-locations (location-1 location-2)
  "Check whether two location paths are similar, i.e. they contain the same sequence of object classes."
  (loop for place-1 in location-1
        for place-2 in location-2
        always (eql (type-of place-1) (type-of place-2))))

(defun find-location (object place)
  "Get the full path to the given object, starting from the given place."
  (if (and (subtypep (type-of object) 'list-of-kitchen-entities)
           (subtypep (type-of place) 'kitchen-state))
    (list place (counter-top place)) ; the items of list-of-entities are always considered to be on the countertop
    (cond ((loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return t))
           (loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return (list place))))
          (t
           (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
               do (let ((location (find-location object el)))
                    (when location
                      (return (append (list place) location)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions to Check Kitchen Entity Equality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-slotnames (item &optional ignore)
  "Get the names of all slots that are available on the given item, excluding slot names present in the ignore list."
  (let* ((classlots  (closer-mop:class-slots (class-of item)))
         (slotnames  (mapcar #'closer-mop:slot-definition-name classlots)))
    (remove-if #'(lambda (slotname) (member slotname ignore))
             slotnames)))

(defun similar-entities (entity-1 entity-2 &optional (ignore '(id persistent-id)))
  "Convenience function to check whether two kitchen entities are similar by checking equality without taking any IDs into account."
  (equal-ontology-objects entity-1 entity-2 ignore))

(defgeneric equal-ontology-objects (object-1 object-2 &optional ignore)
  (:documentation "Returns t if object-1 and object-2 are equal when comparing all slots, except the ones contained in the ignore list."))

(defmethod equal-ontology-objects ((object-1 symbol) (object-2 symbol) &optional ignore)
  (eql object-1 object-2))

(defmethod equal-ontology-objects ((object-1 number) (object-2 number) &optional ignore)
  (= object-1 object-2))

(defmethod equal-ontology-objects ((object-1 string) (object-2 string) &optional ignore)
  (equalp object-1 object-2))

; order doesn't matter for lists in any of our current ontology objects
(defmethod equal-ontology-objects ((object-1 list) (object-2 list) &optional ignore)
  (and (= (length object-1) (length object-2))
       (not (set-difference object-1 object-2
                       :test #'(lambda (el1 el2) (equal-ontology-objects el1 el2 ignore))))))

(defmethod equal-ontology-objects (object-1 object-2 &optional ignore)
  (and (equal (class-of object-1) (class-of object-2))
       (let* ((o1-slotnames (get-slotnames object-1 ignore)))
         (loop for o1-slotname in o1-slotnames
             always (equal-ontology-objects
                     (slot-value object-1  o1-slotname)
                     (slot-value object-2  o1-slotname)
                     ignore)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions for Execution Time Computation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-execution-time (bindings)
  "Compute the maximum time it takes to execute a recipe, i.e., find the time it takes to make all bindings available."
  (apply #'max (remove nil (mapcar #'irl::available-at bindings))))
