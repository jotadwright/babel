(in-package :visual-dialog)

;; -----------------
;; QUERY primtive ;;
;; -----------------

;(export '(query))

         

(defprimitive query ((target-category attribute)
                     (source-object-set world-model)
                     (scene pathname-entity)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object, compute the target category
  ((source-object-set attribute scene => target-category)
   
   (if (equal (length (collect-objects-from-world-model source-object-set)) 0)
     (bind (target-category 1.0 (attribute-none attribute ontology)))
     (bind (target-category 1.0 (query-object-attribute (first (collect-objects-from-world-model source-object-set)) attribute ontology)))))
  
  ;; second case; given source-object and target-category, compute the attribute
  ((source-object-set target-category scene => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-category
                        (query-object-attribute (first (objects source-object-set)) attr ontology)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-object-set scene => target-category attribute)
   (loop for attr in (get-data ontology 'attributes)
         for target-cat = (query-object-attribute (first (objects source-object-set)) attr ontology)
         when target-cat
         do (bind (attribute 1.0 attr)
                  (target-category 1.0 target-cat))))

  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-object-set attribute target-category scene =>)
   (equal-entity target-category (query-object-attribute (first (objects source-object-set)) attribute ontology)))
  :primitive-inventory *symbolic-primitives*)



(defgeneric query-object-attribute (object attribute ontology)
  (:documentation "Extract the attribute from the object and
   get the corresponding category from the ontology"))

(defmethod query-object-attribute ((object object)
                                   (attribute-category attribute-category)
                                   ontology)
  "Given an object and an attribute; get the attribute
   from the object and create a category from it."
  (let* ((attr (attribute attribute-category))
         (spec-attr (cdr (assoc (intern (string attr) "KEYWORD") (attributes object)))))
    "yellow and cyan are bgcolors AND colors, so make sure that the right attribute-category is made"
    (if (not (and (or (eq spec-attr 'yellow)
                      (eq spec-attr 'cyan))
                  (eq attr 'bgcolor)))
      (find-entity-by-id ontology spec-attr)
      (make-instance 'bgcolor-category :id spec-attr :bgcolor spec-attr))))
  

 
(defmethod attribute-none ((attribute-category attribute-category)
                           ontology)
  (let ((none nil))
    (if (equal (attribute attribute-category) 'shape)
      (loop for attr in (get-data ontology 'shapes)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    
    (if (equal (attribute attribute-category) 'size)
      (loop for attr in (get-data ontology 'sizes)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    (if (equal (attribute attribute-category) 'color)
      (loop for attr in (get-data ontology 'colors)
            do (if (equal (id attr) 'none)
                 (setf none attr)))
      )
    
    (if (equal (attribute attribute-category) 'material)
      (loop for attr in (get-data ontology 'materials)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    none))

