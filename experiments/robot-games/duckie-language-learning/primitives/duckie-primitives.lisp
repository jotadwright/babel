(in-package :duckie-language-learning)

;; --------------
;; + Primitives +
;; --------------

;; Primitives are collected in a primitive inventory
(def-irl-primitives duckie-primitive-inventory
  :primitive-inventory *duckie-primitives*)

;; simulation primitives
(def-irl-primitives duckie-primitive-inventory
  :primitive-inventory *duckie-simulation-primitives*)

;; physical demo primitives
(def-irl-primitives duckie-primitive-inventory
  :primitive-inventory *duckie-world-primitives*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; FILTER ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-by-category (set category)
  (let ((filtered-objects
         (loop for object in (objects set)
               for object-value
               = (typecase category
                   (zone-category (zone object))
                   (building-function-category (if (eql (type-of object) 'duckie-building)
                                                        (building-function object)))
                   (color-category (if (or (eql (type-of object) 'duckie-building )
                                           (eql (type-of object) 'duckie-car ))
                                       (color object)))
                  ;;  (rfid-category (if (or (eql (type-of object) 'duckie-building )
                  ;;                         (eql (type-of object) 'duckie-car ))
                  ;;                      (rfid object)))
                   (object-type-category (type-of object)))
               when (eql object-value (category category))
               collect object)))
    (if filtered-objects
      (make-instance 'object-set :objects filtered-objects)
      (make-instance 'object-set :id (make-id 'empty-set)))))

;; filter a set of objects based on some category
(defprimitive filter ((target-set object-set)
                      (source-set object-set)
                      (category duckie-category))

  ;; first case: given set and category, calculate set of items from that category
  ((source-set category => target-set)
   (let ((filtered-set (filter-by-category source-set category)))
     (bind (target-set 1.0 filtered-set))))
  
  ;; third case: if given source-set, compute pairs of target-set and category
  ((source-set => target-set category)
   (let ((categories (append
                      (get-data ontology 'colors)
                      (get-data ontology 'object-types)
                      (get-data ontology 'building-functions)
                      (get-data ontology 'rfids))))
     (loop for cat in categories
           for computed-set = (filter-by-category source-set cat)
           if computed-set
           do (bind (category 1.0 cat)
                    (target-set 1.0 computed-set))
           else
           do (bind (category 1.0 cat)
                    (target-set 1.0 (make-instance 'object-set
                                                   :id (make-id 'empty-set)))))))
 :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; UNIQUE ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive unique ((target-object duckie-object)
                      (source-set object-set))
  ;; first case; given source set, compute target object
  ((source-set => target-object)
   (when (length= (objects source-set) 1)
     (bind (target-object 1.0 (first (objects source-set))))))

  ;; second case; given a target object, compute the source set
  ;; this is simply a set with just the object in it
  ((target-object => source-set)
   (bind (source-set 1.0 (make-instance 'object-set :objects (list target-object)))))

  ;; third case; given source set and target object
  ;; check for consistency
  ((source-set target-object =>)
   (and (length= (objects source-set) 1)
        (equal-entity target-object (first (objects source-set)))))
  :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; EXIST ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive exist ((bool boolean-category)
                     (set object-set))
  ;;First case; given source set, compute target object
  ((set => bool)
   (let* ((booleans (find-data ontology 'bools))
          (target (if (length> (objects set) 0)
                    (find-entity-by-id booleans 'true)
                    (find-entity-by-id booleans 'false))))
     (bind (bool 1.0 target))))

  ;;Second case; enforce consistency of source set and target bool
  ((set bool =>)
   (let* ((booleans (find-data ontology 'bools))
          (target (if (length> (objects set) 0)
                    (find-entity-by-id booleans 'true)
                    (find-entity-by-id booleans 'false))))
     (equal-entity bool target)))

  ;;Third case; target-bool known, source-set unknown
  ;;Unnecessary?
  :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; COUNT ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive count ((target-num number-category)
                     (source-set object-set))
  ;; first case; given source set, count objects
  ((source-set => target-num)
   (bind (target-num 1.0 (make-instance 'number-category
                                        :id (intern (upcase (length (objects source-set)))) ;;needs to be a symbol for feedback from capi
                                        :num (intern (upcase (length (objects source-set))))))))

  ;; second case; given target-num, compute all possible source sets
  ;; might be unnecessary but still added it nonetheless
  ((target-num => source-set)
   (let* ((context (get-data ontology 'context))
          (possible-source-sets
           (permutations-of-length context (num target-num))))
     (loop for set in possible-source-sets
           for num = (make-instance 'number-category :id  (intern (upcase (length set))))
           do (bind (source-set 1.0 num)))))
  
  ;; third case; given source and target, check consistency
  ((source-set target-num =>)
   (= (num target-num) (length (objects source-set))))
  :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; QUERY ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod query-object-attribute ((object duckie-object)
                                   (attribute-category attribute-category)
                                   ontology)
  "Given an object and an attribute; get the attribute
   from the object and create a category from it."
  (case (category attribute-category)
    (color (find (color object) (get-data *ontology* 'colors) :key #'category))
    (when (type-of object 'duckie-building)
      (building-function (find (building-function object) (get-data *ontology* 'building-functions) :key #'category)))
    (rfid (find (rfid object) (get-data *ontology* 'rfids) :key #'category))
    ;(coordinate (find (coordinates object) (get-data *ontology* 'coordinates) :key #'category))
    ))

;; query some property of an object
(defprimitive query ((target-category duckie-category)
                     (source-object duckie-object)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object,
  ;; compute the target category
  ((source-object attribute => target-category)
   (bind (target-category 1.0 (query-object-attribute source-object attribute *ontology*))))

  ;; second case; given source-object and target-category,
  ;; compute the attribute
  ((source-object target-category => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-category
                        (query-object-attribute source-object attr *ontology*)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-object => target-category attribute)
   (loop for attr in (get-data *ontology* 'attributes)
         for target-cat = (query-object-attribute source-object attr *ontology*)
         when target-cat
         do (bind (attribute 1.0 attr)
                  (target-category 1.0 target-cat))))

  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-object attribute target-category =>)
   (equal-entity target-category (query-object-attribute source-object attribute *ontology*)))
  :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; GET-ZONE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive get-zone ((zone zone-category)
                        (object duckie-object))
  ;;First case; given source set, compute target object
  ((object => zone)
   (bind (zone 1.0 (find (zone object) (get-data *ontology* 'zones) :key #'category))))
  ;;Second case; enforce consistency of source set and target bool
  ((zone object =>)
     (equal-entity (zone zone) (zone object)))

  ;;Third case; target-bool known, source-set unknown
  ;;Unnecessary?
  :primitive-inventory (*duckie-simulation-primitives* *duckie-world-primitives*))
