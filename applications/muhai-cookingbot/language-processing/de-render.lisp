(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; De-rendering an utterance together with the accessible-entities of a PDM ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance string)
                      (mode (eql :de-render-recipe-utterance))
                      &key world-state
                      &allow-other-keys)
  "De-renders an utterance and accessible entities of the PDM."
  (let* ((root-unit `(root
                      ,@(cdr (get-root (left-pole-structure (de-render utterance :de-render-string-meets))))
                      (subunits (accessible-entities))))
         (accessible-entity-units (loop for accessible-entity in (accessible-entities world-state)
                                       collect (make-ts-unit-from-entity-id accessible-entity)))
         (accessible-entities-unit `(accessible-entities
                                     (subunits ,(mapcar #'unit-name accessible-entity-units)))))
    (make-instance 'coupled-feature-structure 
		   :left-pole `(,root-unit
                                ,accessible-entities-unit
                                ,@accessible-entity-units)
		   :right-pole '((root)))))

(defun make-ts-unit-from-entity-id (entity-irl-binding)
  (let* ((binding-variable (var entity-irl-binding))
         (entity (value entity-irl-binding))
         (unit-name (id entity)))
    `(,unit-name
      (binding-variable ,binding-variable)
      ,@(make-fs-from-entity entity))))

(defun make-fs-from-entity (entity)
  (cond ((subtypep (type-of entity) 'entity)
         (let* ((ontological-class (type-of entity))
                (ontological-types (set-difference
                                    (mapcar #'class-name
                                            (all-superclasses (find-class ontological-class)))
                                    (mapcar #'class-name (all-superclasses (find-class 'entity)))))
                (other-properties (loop for slot in (closer-mop:class-slots (class-of entity))
                                        for slot-name = (closer-mop:slot-definition-name slot)
                                        for slot-value = (slot-value entity slot-name)
                                        collect (list slot-name (make-fs-from-entity slot-value)))))
           `((ontological-class ,ontological-class)
             (ontological-types ,ontological-types)
             (properties ,other-properties))))
        ((listp entity)
         (loop for el in entity
               collect (make-fs-from-entity el)))
        (t
         entity)))

