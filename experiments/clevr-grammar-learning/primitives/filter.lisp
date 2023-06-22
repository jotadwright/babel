;;;; filter.lisp

(in-package :cgl)

;; ------------------
;; FILTER primtive ;;
;; ------------------

;(export '(filter))

(defgeneric filter-by-category (set category)
  (:documentation "Filter the set by the given category."))

(defmethod filter-by-category ((set clevr-object-set)
                               (shape-category shape-category))
  "Filter the set by the given shape category.
   If the shape is 'thing', return the entire set."
  (if (eq (shape shape-category) 'thing)
    set
    (let ((filtered-objects (loop for object in (objects set)
                                  if (eq (shape object) (shape shape-category))
                                  collect object)))
      (when filtered-objects
        (make-instance 'clevr-object-set :objects filtered-objects)))))

(defmethod filter-by-category ((set clevr-object-set)
                               (size-category size-category))
  "Filter the set by the given size category."
  (let ((filtered-objects (loop for object in (objects set)
                                if (eq (cw::size object) (cw::size size-category))
                                collect object)))
      (when filtered-objects
        (make-instance 'clevr-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set clevr-object-set)
                               (color-category color-category))
  "Filter the set by the given color category."
  (let ((filtered-objects (loop for object in (objects set)
                                if (eq (color object) (color color-category))
                                collect object)))
      (when filtered-objects
        (make-instance 'clevr-object-set :objects filtered-objects))))

(defmethod filter-by-category ((set clevr-object-set)
                               (material-category material-category))
  "Filter the set by the given material category."
  (let ((filtered-objects (loop for object in (objects set)
                                if (eq (material object) (material material-category))
                                collect object)))
      (when filtered-objects
        (make-instance 'clevr-object-set :objects filtered-objects))))

(defprimitive filter ((target-set clevr-object-set)
                      (source-set clevr-object-set)
                      (category attribute))
  ;; first case: if given source-set and category, compute target-set
  ((source-set category => target-set)
   (let ((computed-set (filter-by-category source-set category)))
     (if computed-set
       (bind (target-set 1.0 computed-set))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))
  
  ;; second case: if given source-set and target-set, compute category
  ((source-set target-set => category)
   (let ((computed-category
          (find-if #'(lambda (cat) (equal-entity
                                    target-set
                                    (filter-by-category source-set cat)))
                   (append
                    (get-data ontology 'shapes)
                    (get-data ontology 'sizes)
                    (get-data ontology 'colors)
                    (get-data ontology 'materials)))))
     (when computed-category
       (bind (category 1.0 computed-category)))))

  ;; third case: if given source-set, compute pairs of target-set and category
  ((source-set => target-set category)
   (let ((categories (append
                      (get-data ontology 'shapes)
                      (get-data ontology 'sizes)
                      (get-data ontology 'colors)
                      (get-data ontology 'materials))))
     (loop for cat in categories
           for computed-set = (filter-by-category source-set cat)
           if computed-set
           do (bind (category 1.0 cat)
                    (target-set 1.0 computed-set))
           else
           do (bind (category 1.0 cat)
                    (target-set 1.0 (make-instance 'clevr-object-set
                                                   :id (make-id 'empty-set)))))))

  ;; fourth case: if given source-set, target-set and category, check for consistency
  ((source-set target-set category =>)
   (equal-entity target-set (filter-by-category source-set category)))
  :primitive-inventory *clevr-primitives*)

