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
   (let ((objs (collect-objects-from-world-model source-object-set)))
     (if (equal (length objs) 0)
       (bind (target-category 1.0 (attribute-none attribute ontology)))
       (loop for attr in (query-object-attribute (first objs) attribute ontology)
             do (bind (target-category 1.0 attr))))))
  
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

#|  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-object-set scene => target-category attribute)
   (loop for attr in (get-data ontology 'attributes)
         for target-cat = (query-object-attribute (first (objects (object-set (first (set-items source-object-set))))) attr ontology)
         when target-cat
         do (bind (attribute 1.0 attr)
                  (target-category 1.0 target-cat))))|#

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
         (spec-attr (if (cdr (assoc (intern (string attr) "KEYWORD") (attributes object)))
                      (cdr (assoc (intern (string attr) "KEYWORD") (attributes object)))
                      ;(cdr (assoc (intern (last-elt (split-sequence::split-sequence #\- (string attr))) "KEYWORD") (attributes object)))
                      (cdr (assoc (intern (format nil "GQA-~a" (string attr)) "KEYWORD") (attributes object)))
                      ))
         )
    "yellow and cyan are bgcolors AND colors, so make sure that the right attribute-category is made"
    (if (not spec-attr)
      (list (make-instance (intern (upcase (format nil "~a-category" (symbol-name (id attribute-category)))) "CLEVR-DIALOG-GRAMMAR")
                     :id 'none
                     (intern (upcase (format nil "~a" (symbol-name (id attribute-category)))) "KEYWORD") 'none))
      (if (not (and (or (eq spec-attr 'yellow)
                        (eq spec-attr 'cyan))
                    (eq attr 'bgcolor)))
        (find-entities-by-id ontology spec-attr)
        ;(find-entity-by-id ontology spec-attr)
        (list (make-instance 'bgcolor-category :id spec-attr :bgcolor spec-attr))))))
  

 
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
    (if (equal (attribute attribute-category) 'digit)
      (loop for attr in (get-data ontology 'digits)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    (if (equal (attribute attribute-category) 'bgcolor)
      (loop for attr in (get-data ontology 'bgcolors)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    (if (equal (attribute attribute-category) 'style)
      (loop for attr in (get-data ontology 'styles)
            do (if (equal (id attr) 'none)
                 (setf none attr))))
    none))


(defgeneric find-entities-by-id (thing id &key &allow-other-keys)
  (:documentation "Finds an entity in thing by its id"))

(defmethod find-entities-by-id ((thing t) (id symbol) &key (type 'entity))
  (declare (ignore type))
  nil)

(defmethod find-entities-by-id ((entity entity) (id symbol) &key (type 'entity))
  (when (and (irl::irl-equal (id entity) id)
             (typep entity type))
    entity))

(defmethod find-entities-by-id ((blackboard blackboard) (id symbol) &key (type 'entity))
  (loop for field in (data-fields blackboard)
        for entity = (find-entity-by-id (cdr field) id :type type)
        if entity
          collect entity))


(defmethod find-entities-by-id ((cons cons) (id symbol) &key (type 'entity))
  (if (and (typep (car cons) 'entity)
           (irl::irl-equal (id (car cons)) id)
           (typep (car cons) type))
    (car cons)
    (or (find-entity-by-id (car cons) id :type type)
        (find-entity-by-id (cdr cons) id :type type))))


