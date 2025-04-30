(in-package :clg)

;; --------------------
;; + filter primitive +
;; --------------------

(defprimitive filter ((target-set clevr-object-set)
                      (source-set clevr-object-set)
                      (category attribute))
  ;; first case: if given source-set and category, compute target-set
  ((source-set category => target-set)
   (let ((computed-set (filter-by-concept source-set category ontology)))
     (if computed-set
       (bind (target-set 1.0 computed-set))
       (bind (target-set 1.0 (make-instance 'clevr-object-set :id (make-id 'empty-set)))))))
   
  :primitive-inventory *clevr-primitives*)


;; filter by category
(defmethod filter-by-concept ((set clevr-object-set)
                              (shape-concept shape-category)
                              (ontology blackboard))
  "Filter the set by the given shape category."
  (let* ((concepts (get-data ontology 'shape-concept))
         (filtered-objects (loop for entity in (objects set)
                                 if (eq (id shape-concept) (id (find-best-concept concepts entity)))
                                   collect entity)))
    (when filtered-objects
      (make-instance 'clevr-object-set :objects filtered-objects))))

;; Utility functions
(defun find-best-concept (concepts entity)
  (loop with best-concept = nil
        with best-similarity = nil
        for concept in concepts
        for similarity = (concept-representations::concept-entity-similarity (meaning concept) entity)
        do (format t "~% -> entity ~a has shape: ~a, checking ~a | sim ~,3f" (id entity) (shape entity) (id (meaning concept)) similarity)
        when (or (null best-concept)
                 (> similarity best-similarity))
          do (setf best-concept concept
                   best-similarity similarity)
        finally (return best-concept)))


#|(defgeneric filter-by-category (set category)
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
        (make-instance 'clevr-object-set :objects filtered-objects))))|#

;; second case: if given source-set and target-set, compute category
#|((source-set target-set => category)
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
|#