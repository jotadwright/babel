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
                    (target-category sim target-cat))))
  
  :primitive-inventory *clevr-primitives*)

;; Utilities
(defmethod query-object-attribute ((object clevr-object)
                                   (attribute-category attribute-category)
                                   ontology)
  (let* ((candidate-concepts (get-candidate-concepts ontology attribute-category))
         (res (find-best-concept candidate-concepts object))
         (best-concept (car res))
         (similarity (cdr res)))
    (cons (find-entity-by-id ontology (id best-concept)) similarity)))
