;;;; query.lisp

(in-package :clevr-primitives)

;; -----------------
;; QUERY primtive ;;
;; -----------------

;(export '(query))

(defgeneric query-object-attribute (object attribute ontology)
  (:documentation "Extract the attribute from the object and
   get the corresponding category from the ontology"))

(defmethod query-object-attribute ((object clevr-object)
                                   (attribute-category attribute-category)
                                   ontology)
  "Given an object and an attribute; get the attribute
   from the object and create a category from it."
  (case (attribute attribute-category)
    (shape (find-entity-by-id ontology (shape object)))
    (size (find-entity-by-id ontology (size object)))
    (color (find-entity-by-id ontology (color object)))
    (material (find-entity-by-id ontology (material object)))))

(defprimitive query ((target-category attribute)
                     (source-object clevr-object)
                     (scene pathname-entity)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object, compute the target category
  ((scene source-object attribute => target-category)
   (bind (target-category 1.0 (query-object-attribute source-object attribute ontology))))

  ;; second case; given source-object and target-category, compute the attribute
  ((scene source-object target-category => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-category
                        (query-object-attribute source-object attr ontology)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((scene source-object => target-category attribute)
   (loop for attr in (get-data ontology 'attributes)
         for target-cat = (query-object-attribute source-object attr ontology)
         when target-cat
         do (bind (attribute 1.0 attr)
                  (target-category 1.0 target-cat))))

  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((scene source-object attribute target-category =>)
   (equal-entity target-category (query-object-attribute source-object attribute ontology)))
  :primitive-inventory *clevr-primitives*)


