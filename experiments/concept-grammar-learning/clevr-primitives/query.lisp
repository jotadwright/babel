(in-package :clg)

;; -------------------
;; + QUERY primitive +
;; -------------------


(defprimitive query ((target-category attribute)
                     (source-object clevr-object)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object, compute the target category
  ((source-object attribute => target-category)
   (let* ((res (query-object-attribute source-object attribute ontology))
          (feature (car res)))
   (bind (target-category 1.0 feature))))

  ;; second case; given source-object, compute pairs of attribute and target-category
  ((source-object => attribute target-category)
   (loop for attr in (get-data ontology 'cw::attributes)
         for (target-cat . sim) = (query-object-attribute source-object attr ontology)
         when target-cat
           do (bind (attribute 1.0 attr)
                    (target-category sim target-cat)))
   )
  
  :primitive-inventory *clevr-primitives*)

;; Utilities
(defmethod query-object-attribute ((object clevr-object)
                                   (attribute-category attribute-category)
                                   ontology)
  (multiple-value-bind (candidates concepts) (get-attribute-candidates ontology attribute-category)
    (let* ((res (find-best-concept concepts object))
           (best-concept (car res))
           (similarity (cdr res)))
      (cons (find-entity-by-id ontology (id best-concept)) similarity))))

(defun get-attribute-candidates (ontology attribute-category)
  (cond ((eq (id attribute-category) 'clevr-world::color)
         (values (get-data ontology 'clevr-world::colors) (get-data ontology 'clg::color-concept)))
        ((eq (id attribute-category) 'clevr-world::material)
         (values (get-data ontology 'clevr-world::materials) (get-data ontology 'clg::material-concept)))
        ((eq (id attribute-category) 'clevr-world::size)
         (values (get-data ontology 'clevr-world::sizes) (get-data ontology 'clg::size-concept)))
        ((eq (id attribute-category) 'clevr-world::shape)
         (values (get-data ontology 'clevr-world::shapes) (get-data ontology 'clg::shape-concept)))))


