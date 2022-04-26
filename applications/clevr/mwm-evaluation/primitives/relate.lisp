;;;; relate.lisp

(in-package :mwm-evaluation)

;; ------------------
;; RELATE primtive ;;
;; ------------------
;; Find the set of objects that are in the given spatial relation to an object

;(export '(relate))

;; Return xpos when concept is left or right and ypos when concept is front or behind
(defun get-relation-type (concept)
  (case (id concept)
                ('left 'x-spatial-relations)
                ('right 'x-spatial-relations)
                ('front 'yz-spatial-relations)
                ('behind 'yz-spatial-relations)))

;; Shifts the middle of the relevant axis to the point of the reference object
;; shift = ref - middle axis
(defun get-shifts (reference-object)
  `((:x-shift . ,(- (mwm::get-attr-val reference-object 'mwm::xpos) 240))
    (:y-shift . ,(- (mwm::get-attr-val reference-object 'mwm::ypos) 160))
    (:z-shift . ,(- (mwm::get-attr-val reference-object 'mwm::zpos) 11))))

;; Shifts the concept's prototypical value for the relevant axis and makes a new concept out of it (with the same id)
(defun shift-concept (concept shifts)
  (let* ((shifted-prototypes (loop for prototype in (meaning concept) 
                                   for shifted-prototype = (cond ((eql (attribute prototype) 'mwm::xpos) (shift-prototype prototype (rest (assoc :x-shift shifts))))
                                                                 ((eql (attribute prototype) 'mwm::ypos) (shift-prototype prototype (rest (assoc :y-shift shifts))))
                                                                 ((eql (attribute prototype) 'mwm::zpos) (shift-prototype prototype (rest (assoc :z-shift shifts))))
                                                                 (t  prototype))
                                   collect shifted-prototype))
         (new-concept (make-instance 'spatial-concept :id (id concept) :form (form concept) 
                            :meaning shifted-prototypes)))
    new-concept))

;; shifts the prototypical value by adding the difference between the reference object and the middle of the axis to it
;; shifted-value = value + (ref - middle-of-axis)
(defun shift-prototype (prototype shift)
  (let* ((new-prototype (mwm::my-copy-object prototype :concept (concept prototype)))
         (new-proto-value (+ (value prototype) shift)))
    (setf (value new-prototype) new-proto-value)
    new-prototype))
                                   
(defgeneric apply-spatial-relation (object spatial-relation-category context ontology)
  (:documentation "Apply the spatial relation to the object"))

(defmethod apply-spatial-relation ((object mwm::mwm-object)
                                   (concept spatial-concept) 
                                   (scene mwm::mwm-object-set) 
                                   (ontology blackboard)) 

  (let* ((relation-type (get-relation-type concept))  ;groups left and right and front and behind together
         (shifts (get-shifts object)) ;determines with which amount the concept needs to be shifted
         (relevant-concepts (get-data ontology relation-type))  
         (shifted-concepts (loop for concept in relevant-concepts 
                                 for shifted-concept = (shift-concept concept shifts)
                                 collect shifted-concept))
         (context-objects (remove (find-entity-by-id (objects scene) (id object))(objects scene)))
         (related-objects (loop for context-item in context-objects
                           for best-category = (find-best-category context-item shifted-concepts) ;finds the best suited concept for all context-objects
                           if (eql (id best-category) (id concept)) ;context objects are added to related-objects when their best suited concept is the queried one
                           collect context-item))) 
    (when related-objects 
      (make-instance 'mwm::mwm-object-set :objects related-objects))))



(defprimitive relate ((target-set mwm::mwm-object-set)
                      (source-object mwm::mwm-object)
                      (segmented-scene mwm::mwm-object-set)
                      (scene pathname-entity)
                      (spatial-relation spatial-concept))
  ;; first case; given source-object and spatial relation, compute the target set
  ((scene segmented-scene source-object spatial-relation => target-set)
   (let ((related-set (apply-spatial-relation
                       source-object
                       spatial-relation
                       segmented-scene ontology)))
     (if related-set
       (bind (target-set 1.0 related-set))
       (bind (target-set 1.0 (make-instance 'mwm::mwm-object-set :id (make-id 'empty-set)))))))
  ;; second case; given source-object and target set, compute the spatial relation
  ((scene segmented-scene source-object target-set => spatial-relation)
   (let ((computed-relation
          (find-if #'(lambda (relation)
                       (equal-entity
                        target-set
                        (apply-spatial-relation source-object relation segmented-scene ontology)))
                   (get-data ontology 'spatial-relations))))
     (when computed-relation
       (bind (spatial-relation 1.0 computed-relation)))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((scene segmented-scene source-object => target-set spatial-relation)
   (loop for relation in (get-data ontology 'spatial-relations)
         for set = (apply-spatial-relation source-object relation segmented-scene ontology)
         when set
         do (bind (target-set 1.0 set)
                  (spatial-relation 1.0 relation))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((scene segmented-scene source-object target-set spatial-relation =>)
   (equal-entity target-set (apply-spatial-relation source-object spatial-relation segmented-scene ontology)))
  :primitive-inventory *mwm-primitives*)


;;--------------------------------------------------;;
;; Testing different apply-spatial-relation methods ;;
;;--------------------------------------------------;;

;; 1: Without shifting (accuracy: 86%)
;; Related objects have a higher similarity to the spatial-concept than the queried object
#|(defmethod apply-spatial-relation ((object mwm::mwm-object)
                                   (spatial-relation-category spatial-concept)
                                   (context mwm::mwm-object-set))
  (let ((related-objects (loop for context-item in (objects context)
                               if (> (weighted-similarity context-item spatial-relation-category)
                     (weighted-similarity object spatial-relation-category))
                         collect context-item)))

    (when related-objects
    (make-instance 'mwm::mwm-object-set :objects related-objects))))|#


;; 2: Shift the objects and see if context object is more left than the reference object (accuracy: 88%)
#|(defmethod apply-spatial-relation ((reference-object mwm::mwm-object) (concept spatial-concept) (context-objects mwm::mwm-object-set) (ontology blackboard))
  (let* ((axis (get-axis concept))
         (shift (get-shift reference-object axis))
         (shifted-context-objects (loop for context-object in (objects context-objects)
                                        for shifted-context-object = (shift-object context-object shift axis)
                                        collect shifted-context-object))
         (related-objects (loop for shifted-item in shifted-context-objects
                                for context-item = (find-entity-by-id (objects context-objects) (id shifted-item))
                                for shifted-reference = (shift-object reference-object shift axis)
                                if (> (weighted-similarity shifted-item concept) (weighted-similarity shifted-reference concept))
                                collect context-item)))
    (when related-objects 
      (make-instance 'mwm::mwm-object-set :objects related-objects))))|#

#|(defun shift-object (object shift axis)
  (let* ((new-attributes (loop for attribute-proto-cons in (attributes object)
                               for attribute = (car attribute-proto-cons)
                               for proto-value = (cdr attribute-proto-cons)
                               for shifted-proto-value = (case axis
                                                           ('xpos (- (mwm::get-attr-val object 'mwm::xpos) shift))
                                                           ('ypos (- (mwm::get-attr-val object 'mwm::ypos) shift)))
                               for shifted-attribute-cons = (case axis
                                                         ('xpos (if (eql attribute 'mwm::xpos) (cons attribute shifted-proto-value) attribute-proto-cons))
                                                         ('ypos (if (eql attribute 'mwm::ypos) (cons attribute shifted-proto-value) attribute-proto-cons)))
                               collect shifted-attribute-cons))
        (shifted-object (make-instance 'mwm::mwm-object :id (id object) :attributes new-attributes)))
    shifted-object))|#

;; 3: Shift the objects and find the best category for each context-object (accuracy: 86%)
#|(defmethod apply-spatial-relation ((reference-object mwm::mwm-object) (concept spatial-concept) (context-objects mwm::mwm-object-set) (ontology blackboard))
  (let* ((axis (get-axis concept))
         (shift (get-shift reference-object axis))
         (shifted-context-objects (loop for context-object in (objects context-objects)
                                        for shifted-context-object = (shift-object context-object shift axis)
                                        collect shifted-context-object))
         (related-objects (loop for shifted-item in shifted-context-objects
                                for context-item = (find-entity-by-id (objects context-objects) (id shifted-item))
                                for best-category = (if (eql axis 'xpos)
                                             (find-best-category shifted-item (get-data ontology 'x-spatial-relations))
                                             (find-best-category shifted-item (get-data ontology 'y-spatial-relations)))
                                if (equal-entity concept best-category)
                                collect context-item)))
    (when related-objects 
      (make-instance 'mwm::mwm-object-set :objects related-objects))))|#

;; 4: Shift the concept and see if context-object is more left than the reference object (accuracy: 88%)
#|(defmethod apply-spatial-relation ((object mwm::mwm-object)
                                   (concept spatial-concept) 
                                   (context-objects mwm::mwm-object-set) 
                                   (ontology blackboard)) 
  (let* ((axis (get-axis concept)) 
         (shift (get-shift object axis)) 
         (shifted-concept (shift-concept concept shift axis)) 
         (related-objects (loop for context-item in (objects context-objects) 
                           if (> (weighted-similarity context-item shifted-concept) (weighted-similarity object shifted-concept))  
                           collect context-item))) 
    (when related-objects 
      (make-instance 'mwm::mwm-object-set :objects related-objects))))|#