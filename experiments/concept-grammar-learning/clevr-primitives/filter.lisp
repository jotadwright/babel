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
     (bind (target-set 1.0 computed-set))))
  ((source-set => category target-set)
   (loop for category2 in (loop for top-cat in (list 'cw::materials 'cw::sizes 'cw::shapes 'cw::colors)
                                append (get-data ontology top-cat))
         for computed-set = (filter-by-concept source-set category2 ontology)
         do (bind (category 1.0 category2)
                  (target-set 1.0 computed-set))))
  :primitive-inventory *clevr-primitives*)

  
;; filter by category
(defmethod filter-by-concept ((source-set clevr-object-set)
                              (category category)
                              (ontology blackboard))
  "Filter the set by the given category."
  (multiple-value-bind (candidates concepts) (get-competing-attributes ontology category)
    (let ((filtered-objects (filter-objects-by-concepts concepts (objects source-set) category)))
      (if filtered-objects
        (make-instance 'clevr-object-set
                       :objects (first filtered-objects)
                       :similarities (cons (list category (second filtered-objects)) (similarities source-set)))
        (make-instance 'clevr-object-set
                       :id (make-id 'empty-set))))))

(defun get-associated-concept (ontology category)
  (gethash category (first (get-data ontology 'all-concepts))))



(defmethod filter-objects-by-concepts (concepts entities category)
  (loop for entity in entities
        for (best-concept . similarity) = (find-best-concept concepts entity)
        when (and best-concept (eq (id category) (id best-concept)))
          collect entity into entities
        collect (cons (id entity) similarity) into similarities
        finally (return (list entities similarities))))

;; Utility functions
(defun find-best-concept (concepts entity)
  (loop with best-concept = nil
        with best-similarity = nil
        for concept in concepts
        for similarity = (concept-representations::concept-entity-similarity (meaning concept) entity)
        ;do (format t "~% -> entity ~a has shape: ~a, checking ~a | sim ~,3f" (id entity) (shape entity) (id concept) similarity)
        when (or (null best-concept)
                 (> similarity best-similarity))
          do (setf best-concept concept
                   best-similarity similarity)
        finally (return (cons best-concept best-similarity))))

(defun get-competing-attributes (ontology category)
  (cond ;; colors
        ((member (id category) (list 'cw::blue 'cw::brown 'cw::cyan 'cw::gray 'cw::green 'cw::purple 'cw::red 'cw::yellow) :test #'eq)
         (values (get-data ontology 'clevr-world::colors) (get-data ontology 'clg::color-concept))
         )
        ;; materials
        ((member (id category) (list 'cw::metal 'cw::rubber) :test #'eq)
         (values (get-data ontology 'clevr-world::materials) (get-data ontology 'clg::material-concept))
         )
        ;; size
        ((member (id category) (list 'cw::small 'cw::large) :test #'eq)
         (values (get-data ontology 'clevr-world::sizes) (get-data ontology 'clg::size-concept))
         )
        ;; shapes
        ((member (id category) (list 'cw::cube 'cw::cylinder 'cw::sphere) :test #'eq)
         (values (get-data ontology 'clevr-world::shapes) (get-data ontology 'clg::shape-concept))
         )))