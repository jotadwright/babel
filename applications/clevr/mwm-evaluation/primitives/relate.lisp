;;;; relate.lisp

(in-package :mwm-evaluation)

;; ------------------
;; RELATE primtive ;;
;; ------------------
;; Find the set of objects that are in the given spatial relation to an object

;(export '(relate))

(defgeneric apply-spatial-relation (object spatial-relation-category context)
  (:documentation "Apply the spatial relation to the object"))

;; Related objects have a higher similarity to the spatial-concept than the queried object
(defmethod apply-spatial-relation ((object mwm::mwm-object)
                                   (spatial-relation-category spatial-concept)
                                   (context mwm::mwm-object-set))
  (let ((related-objects (loop for context-item in (objects context)
                               if (> (weighted-similarity context-item spatial-relation-category)
                     (weighted-similarity object spatial-relation-category))
                         collect context-item)))

    (when related-objects
      (make-instance 'mwm::mwm-object-set :objects related-objects))))


(defprimitive relate ((target-set mwm::mwm-object-set)
                      (source-object mwm::mwm-object)
                      (segmented-scene mwm::mwm-object-set)
                      (scene pathname-entity)
                      (spatial-relation spatial-concept))
  ;; first case; given source-object and spatial relation, compute the target set
  ((scene segmented-scene source-object spatial-relation => target-set)
   (let ((related-set (apply-spatial-relation
                       source-object
                       spatial-relation
                       segmented-scene)))
     (if related-set
       (bind (target-set 1.0 related-set))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))

  #|
  ;; second case; given source-object and target set, compute the spatial relation
  ((scene segmented-scene source-object target-set => spatial-relation)
   (let ((computed-relation
          (find-if #'(lambda (relation)
                       (equal-entity
                        target-set
                        (apply-spatial-relation source-object relation segmented-scene)))
                   (get-data ontology 'spatial-relations))))
     (when computed-relation
       (bind (spatial-relation 1.0 computed-relation)))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((scene segmented-scene source-object => target-set spatial-relation)
   (loop for relation in (get-data ontology 'spatial-relations)
         for set = (apply-spatial-relation source-object relation segmented-scene)
         when set
         do (bind (target-set 1.0 set)
                  (spatial-relation 1.0 relation))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((scene segmented-scene source-object target-set spatial-relation =>)
   (equal-entity target-set (apply-spatial-relation source-object spatial-relation segmented-scene)))
  |#
  :primitive-inventory *mwm-primitives*)
