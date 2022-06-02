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
  (let* ((x-pos (mwm::get-attr-val reference-object 'mwm::xpos))
         (y-pos (mwm::get-attr-val reference-object 'mwm::ypos))
         (z-pos (mwm::get-attr-val reference-object 'mwm::zpos))
         (shifts `((:x-shift . , (if x-pos (- x-pos 240) nil))
                   (:y-shift . , (if y-pos (- y-pos 160) nil))
                   (:z-shift . , (if z-pos (- z-pos 11) nil)))))
    shifts))

;; Shifts the concept's prototypical value for the relevant axis and makes a new concept out of it (with the same id)
(defun shift-concept (concept shifts)
  (let* ((shifted-prototypes
          (loop for prototype in (meaning concept) 
                for shifted-prototype
                  = (cond ((eql (attribute prototype) 'mwm::xpos)
                           (shift-prototype prototype (rest (assoc :x-shift shifts))))
                          ((eql (attribute prototype) 'mwm::ypos)
                           (shift-prototype prototype (rest (assoc :y-shift shifts))))
                          ((eql (attribute prototype) 'mwm::zpos)
                           (shift-prototype prototype (rest (assoc :z-shift shifts))))
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
         (related-objects
          (loop for context-item in context-objects
                  ;finds the best suited concept for all context-objects
                for best-category = (find-best-category context-item shifted-concepts)
                  ;context objects are added to related-objects when their best suited concept is the queried one
                if (eql (id best-category) (id concept)) 
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

