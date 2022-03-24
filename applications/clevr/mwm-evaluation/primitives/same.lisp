;;;; same.lisp

(in-package :mwm-evaluation)

;; ----------------
;; SAME primtive ;;
;; ----------------
;; Find objects that have the same value for an attribute as the given object

;(export '(same))

(defgeneric same-set-by-object-attribute (set object attribute ontology)
  (:documentation "Filter the given set by the attribute of the given object;
   also remove the object itself from this set."))

(defmethod same-set-by-object-attribute ((set mwm::mwm-object-set)
                                         (object mwm::mwm-object)
                                         (attribute-category attribute-category)
                                         (ontology blackboard))
  (let* ((object-attribute (query-object-attribute object attribute-category ontology))
         (consider-set (remove (id object) (objects set) :key #'id))
         (same-set (loop for obj in consider-set
                         when (eq object-attribute (query-object-attribute obj attribute-category ontology))
                         collect obj)))
    (when same-set
      (make-instance 'mwm::mwm-object-set :objects same-set))))


(defprimitive same ((target-set mwm::mwm-object-set)
                    (source-object mwm::mwm-object)
                    (segmented-scene mwm::mwm-object-set)
                    (scene pathname-entity)
                    (attribute attribute-category))
  ;; first case; given source-object and attribute, compute the target-set
  ((scene segmented-scene source-object attribute => target-set)
   (let ((same-set (same-set-by-object-attribute segmented-scene source-object attribute ontology)))
     (if same-set
       (bind (target-set 1.0 same-set))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))

  ;; second case; given source-object and target-set, compute the attribute
  ((scene segmented-scene source-object target-set => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-set
                        (same-set-by-object-attribute segmented-scene source-object attr ontology)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-set
  ((scene segmented-scene source-object => target-set attribute)
   (loop for attr in (get-data ontology 'attributes)
         for set = (same-set-by-object-attribute segmented-scene source-object attr ontology)
         if set
         do (bind (target-set 1.0 set)
                  (attribute 1.0 attr))
         else
         do (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set
                                                 :id (make-id 'empty-set)))
                  (attribute 1.0 attr))))

  ;; fourth case; given source-object, attribute and target set,
  ;; check for consistency
  ((scene segmented-scene source-object attribute target-set =>)
   (equal-entity target-set (same-set-by-object-attribute
                             segmented-scene
                             source-object
                             attribute
                             ontology)))
  :primitive-inventory *mwm-primitives*)

