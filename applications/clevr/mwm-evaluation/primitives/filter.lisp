;;;; filter.lisp

(in-package :mwm-evaluation)

;; ------------------
;; FILTER primtive ;;
;; ------------------
;; Filter set on a given category

;(export '(filter))

(defgeneric filter-by-category (set category ontology)
  (:documentation "Filter the set by the given category."))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (shape-category shape-concept)
                               (ontology blackboard))
  "Filter the set by the given shape category.
   If the shape is 'thing', return the entire set."
  (if (eq (id shape-category) 'thing)
    set
    (let ((filtered-objects
           (loop for object in (objects set)
                 if (equal-entity shape-category
                                  (find-best-category object (get-data ontology 'shapes)))
                 collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects)))))


(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (size-category size-concept)
                               (ontology blackboard))
  "Filter the set by the given size category."
 (let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity size-category
                                                 (find-best-category object (get-data ontology 'sizes)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (color-category color-concept)
                               (ontology blackboard))
  "Filter the set by the given color category."
 (let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity color-category
                                                 (find-best-category object (get-data ontology 'colors)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (material-category material-concept)
                               (ontology blackboard))
  "Filter the set by the given material category."
(let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity material-category
                                                 (find-best-category object (get-data ontology 'materials)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))



(defprimitive filter ((target-set mwm::mwm-object-set)
                      (source-set mwm::mwm-object-set)
                      (scene pathname-entity)
                      (category concept-entity))
  ;; first case: if given source-set and category, compute target-set
  ((scene source-set category => target-set)
   (let ((computed-set (filter-by-category source-set category ontology)))
     (if computed-set
       (bind (target-set 1.0 computed-set))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))

  #|
  ;; second case: if given source-set and target-set, compute category
  ((scene source-set target-set => category)
   (let ((computed-category
          (find-if #'(lambda (cat) (equal-entity
                                    target-set
                                    (filter-by-category source-set cat ontology)))
                   (append
                    (get-data ontology 'shapes)
                    (get-data ontology 'sizes)
                    (get-data ontology 'colors)
                    (get-data ontology 'materials)))))
     (when computed-category
       (bind (category 1.0 computed-category)))))

  ;; third case: if given source-set, compute pairs of target-set and category
  ((scene source-set => target-set category)
   (let ((categories (append
                      (get-data ontology 'shapes)
                      (get-data ontology 'sizes)
                      (get-data ontology 'colors)
                      (get-data ontology 'materials))))
     (loop for cat in categories
           for computed-set = (filter-by-category source-set cat ontology)
           if computed-set
           do (bind (category 1.0 cat)
                    (target-set 1.0 computed-set))
           else
           do (bind (category 1.0 cat)
                    (target-set 1.0 (make-instance 'mwm::mwm-object-set
                                                   :id (make-id 'empty-set)))))))

  ;; fourth case: if given source-set, target-set and category, check for consistency
  ((scene source-set target-set category =>)
   (equal-entity target-set (filter-by-category source-set category ontology)))
  |#
  :primitive-inventory *mwm-primitives*)

