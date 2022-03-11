;;;; filter.lisp

(in-package :mwm-evaluation)

;; ------------------
;; FILTER primtive ;;
;; ------------------
;; Filter set on a given category

;(export '(filter))

;; attach category to an object that yields the highest weighted similarity out of a set of categories
(defun find-best-category (object categories)
  (reduce #'(lambda (cat1 cat2)
              (if (> (weighted-similarity object cat1)
                     (weighted-similarity object cat2))
                cat1 cat2))
          categories))

(defgeneric filter-by-category (set category)
  (:documentation "Filter the set by the given category."))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (shape-category shape-concept))
  "Filter the set by the given shape category.
   If the shape is 'thing', return the entire set."
  (if (eq (id shape-category) 'thing)
    set
    (let ((filtered-objects
           (loop for object in (objects set)
                 if (equal-entity shape-category
                                  (find-best-category object (get-data *my-ontology* 'shapes)))
                 collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects)))))


(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (size-category size-concept))
  "Filter the set by the given size category."
 (let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity size-category
                                                 (find-best-category object (get-data *my-ontology* 'sizes)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (color-category color-concept))
  "Filter the set by the given color category."
 (let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity color-category
                                                 (find-best-category object (get-data *my-ontology* 'colors)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set mwm::mwm-object-set)
                               (material-category material-concept))
  "Filter the set by the given material category."
(let ((filtered-objects (loop for object in (objects set)
                                  if (equal-entity material-category
                                                 (find-best-category object (get-data *my-ontology* 'materials)))
                                collect object)))
      (when filtered-objects
        (make-instance 'mwm::mwm-object-set :objects filtered-objects))))



(defprimitive filter ((target-set mwm::mwm-object-set)
                      (source-set mwm::mwm-object-set)
                      (scene pathname-entity)
                      (category concept-entity))
  ;; first case: if given source-set and category, compute target-set
  ((scene source-set category => target-set)
   (let ((computed-set (filter-by-category source-set category)))
     (if computed-set
       (bind (target-set 1.0 computed-set))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))
  
  ;; second case: if given source-set and target-set, compute category
  ((scene source-set target-set => category)
   (let ((computed-category
          (find-if #'(lambda (cat) (equal-entity
                                    target-set
                                    (filter-by-category source-set cat)))
                   (append
                    (get-data *my-ontology* 'shapes)
                    (get-data *my-ontology* 'sizes)
                    (get-data *my-ontology* 'colors)
                    (get-data *my-ontology* 'materials)))))
     (when computed-category
       (bind (category 1.0 computed-category)))))

  ;; third case: if given source-set, compute pairs of target-set and category
  ((scene source-set => target-set category)
   (let ((categories (append
                      (get-data *my-ontology* 'shapes)
                      (get-data *my-ontology* 'sizes)
                      (get-data *my-ontology* 'colors)
                      (get-data *my-ontology* 'materials))))
     (loop for cat in categories
           for computed-set = (filter-by-category source-set cat)
           if computed-set
           do (bind (category 1.0 cat)
                    (target-set 1.0 computed-set))
           else
           do (bind (category 1.0 cat)
                    (target-set 1.0 (make-instance 'mwm::mwm-object-set
                                                   :id (make-id 'empty-set)))))))

  ;; fourth case: if given source-set, target-set and category, check for consistency
  ((scene source-set target-set category =>)
   (equal-entity target-set (filter-by-category source-set category)))
  :primitive-inventory *mwm-primitives*)

