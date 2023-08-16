(ql:quickload :irl)
(in-package :irl)
;; This file contains the code to run IRL networks in the SHRDLU world. 

;; Part 1: Building blocks of IRL
;; ------------------------------

;; Entities are the building blocks of IRL
;; They are used to represent
;;     1. objects in the agent's environment
;;     2. the agent's knowledge, in terms of semantic concepts/categories
;; The agent's environment is typically called the _context_
;; The agent's knowledge is stored in the _ontology_


(defclass simple-object (entity)
  ((color
    :initarg :color :accessor color :initform nil :type symbol
    :documentation "the color of the object")
   (size
    :initarg :size :accessor size :initform nil :type symbol
    :documentation "the size of the object")
   (shape
    :initarg :shape :accessor shape :initform nil :type symbol
    :documentation "the shape of the object")
   ;; relationships is an alist of (spatial-relation . object-ids)
   ;; for example (left . obj-1 obj-2) means that obj-1 and obj-2
   ;; are left of this object
   (relationships
    :initarg :relationships :accessor relationships :initform nil :type list
    :documentation "the spatial relationships of the object"))
  (:documentation "an object in the world"))

(defmethod make-html-for-entity-details ((obj simple-object) &key)
  "Draw the objects in the web interface"
  `(((div :class "entity-detail")
     ((table)
      ((tr) ((td) "color:") ((td) ,(format nil "~(~a~)" (color obj))))
      ((tr) ((td) "size:") ((td) ,(format nil "~(~a~)" (size obj))))
      ((tr) ((td) "shape:") ((td) ,(format nil "~(~a~)" (shape obj))))
      ,@(loop for (relation . object-ids) in (relationships obj)
              collect `((tr) ((td) ,(format nil "~(~a~):" relation))
                         ((td) ,(if (null object-ids) "/"
                                 (format nil "~{~(~a~)~^,~}" object-ids)))))))))

(defmethod equal-entity ((obj-1 simple-object) (obj-2 simple-object))
  "Objects are equal when their attributes are"
  (and (eql (color obj-1) (color obj-2))
       (eql (size obj-1) (size obj-2))
       (eql (shape obj-1) (shape obj-2))))

(defmethod copy-object-content ((src simple-object) (copy simple-object))
  (setf (id copy) (id src) ;; make sure that copies have the same id!
        (color copy) (color src)
        (size copy) (size src)
        (shape copy) (shape src)
        (relationships copy) (mapcar #'copy-object (relationships src))))



(defclass simple-object-set (entity)
  ((objects
    :initarg :objects :accessor objects :initform nil :type list
    :documentation "a set of objects"))
  (:documentation "a set of objects in the world"))

(defmethod make-html-for-entity-details ((set simple-object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

(defmethod equal-entity ((set-1 simple-object-set) (set-2 simple-object-set))
  (permutation-of? (objects set-1) (objects set-2) :test #'equal-entity))

(defmethod copy-object-content ((src simple-object-set) (copy simple-object-set))
  ;; for sets of objects, the id does not matter
  (setf (objects copy) (mapcar #'copy-object (objects src))))

(defmethod find-entity-by-id ((set simple-object-set) (id symbol))
  (find-entity-by-id (objects set) id))




(defun make-random-simple-object ()
  "Make a random object"
  (let ((possible-shapes '(cube cylinder ball pyramid))
        (possible-sizes '(tiny small large huge))
        (possible-colors '(red blue green yellow purple)))
    (make-instance 'simple-object :id (make-id 'obj)
                   :shape (random-elt possible-shapes)
                   :size (random-elt possible-sizes)
                   :color (random-elt possible-colors))))

(defun make-random-simple-object-set (n)
  "Make a random set of simple objects.
   Randomly decide on spatial relations between the objects
   in terms of left, right, front and back. Objects will
   not be stacked."
  (let* ((all-objects
          (loop repeat n collect (make-random-simple-object)))
         (all-object-ids
          (mapcar #'id all-objects)))
    ;; shuffle the objects. This is their ordering from left to right
    (loop with processed-ids = nil
          for object in (simple-shuffle all-objects)
          ;; all objects already processed are left of this object
          do (push (cons 'left processed-ids)
                   (relationships object))
          ;; all other objects are right of this object
          ;; don't forget to remove the ID of this object as well!
          do (push (cons 'right
                         (set-difference all-object-ids
                                         (cons (id object) processed-ids)))
                   (relationships object))
          do (push (id object) processed-ids))
    ;; shuffle the objects again. This is their ordering from front to back
    (loop with processed-ids = nil
          for object in (simple-shuffle all-objects)
          ;; all objects already processed are in front of this object
          do (push (cons 'front processed-ids)
                   (relationships object))
          ;; all other objects are behind this object
          ;; don't forget to remove the ID of this object as well!
          do (push (cons 'back
                         (set-difference all-object-ids
                                         (cons (id object) processed-ids)))
                   (relationships object))
          do (push (id object) processed-ids))
    ;; add the 'on and 'below relations to each object, but empty
    (loop for object in all-objects
          do (push (cons 'on nil) (relationships object))
          do (push (cons 'below nil) (relationships object)))
    ;; make an object set and return it
    (make-instance 'simple-object-set :objects all-objects)))


(defun make-example-object-set ()
  (let* ((objects (list (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'cube
                                      :size 'tiny
                                      :color 'red)
                       (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'cube
                                      :size 'large
                                      :color 'blue)
                       (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'cylinder
                                      :size 'tiny
                                      :color 'yellow)
                       (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'pyramid
                                      :size 'small
                                      :color 'purple)
                       (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'ball
                                      :size 'huge
                                      :color 'red)
                       (make-instance 'simple-object :id (make-id 'obj)
                                      :shape 'cylinder
                                      :size 'tiny
                                      :color 'green)))
        (all-object-ids
          (mapcar #'id objects)))
    (loop with processed-ids = nil
          for object in objects
          do (push (cons 'front processed-ids)
                   (relationships object))
          do (push (cons 'back
                         (set-difference all-object-ids
                                         (cons (id object) processed-ids)))
                   (relationships object))
          do (push (id object) processed-ids))
    (loop with processed-ids = nil
          for object in objects
          do (push (cons 'left processed-ids)
                   (relationships object))
          do (push (cons 'right
                         (set-difference all-object-ids
                                         (cons (id object) processed-ids)))
                   (relationships object))
          do (push (id object) processed-ids))
    (loop for object in objects
          do (push (cons 'on nil) (relationships object))
          do (push (cons 'below nil) (relationships object)))
  (make-instance 'simple-object-set :objects objects)))
                               


(defclass simple-category (entity)
  ((category
    :initarg :category :accessor category :initform nil :type symbol
    :documentation "the category")))

(defmethod make-html-for-entity-details ((cat simple-category) &key)
  `(((div :class "entity-detail")
     ,(format nil "~(~a~)" (category cat)))))

(defmethod equal-entity ((cat-1 simple-category) (cat-2 simple-category))
  (eql (category cat-1) (category cat-2)))

(defmethod copy-object-content ((src simple-category) (copy simple-category))
  (setf (category copy) (category src)))
  
(defclass color-category (simple-category) ())

(defclass size-category (simple-category) ())

(defclass shape-category (simple-category) ())

(defclass spatial-relation (simple-category) ())

(defclass attribute-category (simple-category) ())

(defclass boolean-category (simple-category) ())









(defun build-initial-ontology ()
  (let ((shapes '(cube cylinder ball pyramid))
        (sizes '(tiny small large huge))
        (colors '(red blue green yellow purple))
        (relations '(left right front back on below))
        (attributes '(shape size color))
        (booleans '(true false))
        (initial-ontology (make-blackboard)))
    (loop for shape in shapes
          for instance = (make-instance 'shape-category
                                        :id shape :category shape)
          do (push-data initial-ontology 'shapes instance))
    (loop for size in sizes
          for instance = (make-instance 'size-category
                                        :id size :category size)
          do (push-data initial-ontology 'sizes instance))
    (loop for color in colors
          for instance = (make-instance 'color-category
                                        :id color :category color)
          do (push-data initial-ontology 'colors instance))
    (loop for relation in relations
          for instance = (make-instance 'spatial-relation
                                        :id relation :category relation)
          do (push-data initial-ontology 'spatial-relations instance))
    (loop for attribute in attributes
          for instance = (make-instance 'attribute-category
                                        :id attribute :category attribute)
          do (push-data initial-ontology 'attributes instance))
    (loop for boolean in booleans
          for instance = (make-instance 'boolean-category
                                        :id boolean :category boolean)
          do (push-data initial-ontology 'booleans instance))
    initial-ontology))

(defun make-new-context (context-size ontology)
  "Make a new context (i.e. a new set of objects) and
  add it to the ontology so IRL primitives can access it"
  (let ((context (make-random-simple-object-set context-size)))
    (set-data ontology 'context context)
    ontology))

(defun make-example-context (ontology)
  "Make a new context (i.e. a new set of objects) and
   add it to the ontology so IRL primitives can access it"
  (let ((context (make-example-object-set)))
    (set-data ontology 'context context)
    ontology))



;; Part 2: Primitive operations
;; ----------------------------

;; IRL's primitives represent basic cognitive operations
;; that can be executed by the agent. Primitives operate
;; over semantic entities, as defined above. They are
;; _predicates_, not functions. Hence, they specify a
;; relation between their arguments. All arguments of a
;; primitive are typed. Unbound arguments, i.e. arguments
;; for which there is not yet a value, are represented
;; through variables. Primitives are linked together into
;; IRL networks (or meaning network) by unifying arguments
;; of primitives.

;; List of available primitives:
;; observe-objects, filter, query, count-set, exist, relate, unique, stack, move

;; Primitives have a number of arguments. All arguments are typed.
;; Primitives can be implemented in multiple directions.
;; Argument before the => are bound and those behind the => are unbound

;; Primitives are collected in a primitive inventory
(def-irl-primitives simple-primitive-inventory
  :primitive-inventory *simple-primitives*)


;; observe-objects
;; ---------------

;; retrieve all objects in the current scene
(defprimitive observe-objects ((context simple-object-set))
  ;;Case 1: context bound
  ((context =>)
   (equal-entity context (get-data ontology 'context)))
  ;;Case 2
  ((=> context)
   (bind (context 1.0 (get-data ontology 'context))))
  :primitive-inventory *simple-primitives*)


;; filter
;; ------

(defun filter-by-category (set category)
  (let ((filtered-objects
         (loop for object in (objects set)
               for object-value
               = (typecase category
                   (color-category (color object))
                   (size-category (size object))
                   (shape-category (shape object)))
               when (eql object-value (category category))
               collect object)))
    (if filtered-objects
      (make-instance 'simple-object-set :objects filtered-objects)
      (make-instance 'simple-object-set :id (make-id 'empty-set)))))

;; filter a set of objects based on some category
(defprimitive filter ((target-set simple-object-set)
                      (source-set simple-object-set)
                      (category simple-category))
  ((source-set category => target-set)
   (let ((computed-set (filter-by-category source-set category)))
     (bind (target-set 1.0 computed-set))))

  ((source-set target-set => category)
   (let ((computed-category
          (find-if #'(lambda (cat)
                       (equal-entity
                        target-set
                        (filter-by-category source-set cat)))
                   (append
                    (get-data ontology 'shapes)
                    (get-data ontology 'sizes)
                    (get-data ontology 'colors)))))
     (when computed-category
       (bind (category 1.0 computed-category)))))

  ((source-set => target-set category)
   (let ((categories (append
                      (get-data ontology 'shapes)
                      (get-data ontology 'sizes)
                      (get-data ontology 'colors))))
     (loop for cat in categories
           for computed-set = (filter-by-category source-set cat)
           do (bind (category 1.0 cat)
                    (target-set 1.0 computed-set)))))

  ((target-set source-set category =>)
   (equal-entity target-set (filter-by-category source-set category)))
  :primitive-inventory *simple-primitives*)

;; query
;; -----

(defmethod query-object-attribute ((object simple-object)
                                   (attribute-category attribute-category)
                                   ontology)
  "Given an object and an attribute; get the attribute
   from the object and create a category from it."
  (case (category attribute-category)
    (shape (find (shape object) (get-data ontology 'shapes) :key #'category))
    (size (find (size object) (get-data ontology 'sizes) :key #'category))
    (color (find (color object) (get-data ontology 'colors) :key #'category))))

;; query some property of an object
(defprimitive query ((target-category simple-category)
                     (source-object simple-object)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object,
  ;; compute the target category
  ((source-object attribute => target-category)
   (bind (target-category 1.0 (query-object-attribute source-object attribute ontology))))

  ;; second case; given source-object and target-category,
  ;; compute the attribute
  ((source-object target-category => attribute)
   (let ((computed-attribute
          (find-if #'(lambda (attr)
                       (equal-entity
                        target-category
                        (query-object-attribute source-object attr ontology)))
                   (get-data ontology 'attributes))))
     (when computed-attribute
       (bind (attribute 1.0 computed-attribute)))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-object => target-category attribute)
   (loop for attr in (get-data ontology 'attributes)
         for target-cat = (query-object-attribute source-object attr ontology)
         when target-cat
         do (bind (attribute 1.0 attr)
                  (target-category 1.0 target-cat))))

  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-object attribute target-category =>)
   (equal-entity target-category (query-object-attribute source-object attribute ontology)))
  :primitive-inventory *simple-primitives*)


;; count-set
;; ---------
;; (this primitive cannot be called 'count', because it is
;;  a build in function in Common Lisp)

;; count the number of objects in a set
(defprimitive count-set ((target-num number)
                         (source-set simple-object-set))
  ;; first case; given source-set, compute target
  ((source-set => target-num)
   (bind (target-num 1.0 (length (objects source-set)))))

  ;; second case; given target-num, compute all possible source sets
  ;; this case creates lots of branches and might be unnecessary
  ((target-num => source-set)
   (let* ((context (get-data ontology 'context))
          (possible-source-sets
           (permutations-of-length context target-num)))
     (loop for set in possible-source-sets
           do (bind (source-set 1.0 set)))))
  
  ;; third case; given source and target, check consistency
  ((source-set target-num =>)
   (= target-num (length (objects source-set))))
  :primitive-inventory *simple-primitives*)


;; exist
;; -----

;; determine whether some set of objects is empty or not
(defprimitive exist ((target-bool boolean-category)
                     (source-set simple-object-set))
  ;; first case; give source-set, compute target-bool
  ((source-set => target-bool)
   (let* ((booleans (find-data ontology 'booleans))
          (target (if (length> (objects source-set) 0)
                    (find-entity-by-id booleans 'true)
                    (find-entity-by-id booleans 'false))))
     (bind (target-bool 1.0 target))))

  ;; second case; given a bool, compute the source set
  ;; this is unnecessary. when bool = nil, return the empty set
  ;; when bool = t; return all possible subsets of the context

  ;; third case; given source-set and target-bool, check consistency
  ((source-set target-bool =>)
   (let* ((booleans (find-data ontology 'booleans))
          (target (if (length> (objects source-set) 0)
                    (find-entity-by-id booleans 'true)
                    (find-entity-by-id booleans 'false))))
     (equal-entity target-bool target)))
  :primitive-inventory *simple-primitives*)


;; unique
;; ------

;; determine if some set of objects contains just a single object
;; if so, return it
(defprimitive unique ((target-object simple-object)
                      (source-set simple-object-set))
  ;; first case; given source set, compute target object
  ((source-set => target-object)
   (when (length= (objects source-set) 1)
     (bind (target-object 1.0 (first (objects source-set))))))

  ;; second case; given a target object, compute the source set
  ;; this is simply a set with just the object in it
  ((target-object => source-set)
   (bind (source-set 1.0 (make-instance 'simple-object-set
                                        :objects (list target-object)))))

  ;; third case; given source set and target object
  ;; check for consistency
  ((source-set target-object =>)
   (and (length= (objects source-set) 1)
        (equal-entity target-object (first (objects source-set)))))
  :primitive-inventory *simple-primitives*)

;; relate
;; ------

(defmethod apply-spatial-relation ((object simple-object)
                                   (relation spatial-relation)
                                   (context simple-object-set))
  (let* ((related-ids
          (rest (assoc (category relation) (relationships object))))
         (related-objects
          (loop for id in related-ids
                for found = (find-entity-by-id context id)
                when found collect found)))
    (if related-objects
      (make-instance 'simple-object-set :objects related-objects)
      (make-instance 'simple-object-set :id (make-id 'empty-set)))))

;; return all objects that have a certain spatial relation
;; with respect to a certain object
(defprimitive relate ((target-set simple-object-set)
                      (source-object simple-object)
                      (spatial-relation spatial-relation))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-object spatial-relation => target-set)
   (let ((related-set (apply-spatial-relation
                       source-object
                       spatial-relation
                       (get-data ontology 'context))))
     (bind (target-set 1.0 related-set))))

  ;; second case; given source-object and target set, compute the spatial relation
  ((source-object target-set => spatial-relation)
   (let* ((context (get-data ontology 'clevr-context))
          (computed-relation
           (find-if #'(lambda (relation)
                        (equal-entity
                         target-set
                         (apply-spatial-relation source-object relation context)))
                    (get-data ontology 'spatial-relations))))
     (bind (spatial-relation 1.0 computed-relation))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((source-object => target-set spatial-relation)
   (let ((context (get-data ontology 'clevr-context)))
     (loop for relation in (get-data ontology 'spatial-relations)
           for set = (apply-spatial-relation source-object relation context)
           do (bind (target-set 1.0 set)
                    (spatial-relation 1.0 relation)))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((source-object target-set spatial-relation =>)
   (let ((context (get-data ontology 'clevr-context)))
     (equal-entity target-set (apply-spatial-relation source-object spatial-relation context))))
  :primitive-inventory *simple-primitives*)

;; stack
;; -----

(defun set-ids-as-object-relations (object relation ids)
  (setf (rest (assoc relation (relationships object))) ids))

(defun push-ids-to-object-relations (object relation ids)
  (setf (rest (assoc relation (relationships object)))
        (append ids (rest (assoc relation (relationships object))))))

(defun get-ids-from-object-relations (object relation)
  (rest (assoc relation (relationships object))))

(defun remove-ids-from-object-relations (object relation ids)
  (setf (rest (assoc relation (relationships object)))
        (set-difference (rest (assoc relation (relationships object))) ids)))

(defun find-id-in-object-relations (object id)
  (loop for (relation . object-ids) in (relationships object)
        when (find id object-ids)
        return relation))

(defun stack-objects (source-set bottom-object top-object)
  ;; first, check constraints
  ;; a ball cannot be the bottom object or the top object
  ;; a pyramid cannot be the bottom object
  ;; (could also add stackable properties to objects)
  ;; the bottom object has to be free (i.e. have nothing on top)
  ;; the top object has to be moveable (i.e. have nothing on top)
  (unless (or (equal-entity bottom-object top-object)
              (eql (shape bottom-object) 'ball)
              (eql (shape top-object) 'ball)
              (eql (shape bottom-object) 'pyramid)
              (get-ids-from-object-relations bottom-object 'on)
              (get-ids-from-object-relations top-object 'on))
    ;; if ok; change spatial relations
    ;; important; make changes in the target set!
    (let* ((target-set (copy-object source-set))
           (target-bottom (find-entity-by-id target-set (id bottom-object)))
           (target-top (find-entity-by-id target-set (id top-object))))
      ;; top needs to be removed from all spatial relationships
      ;; and added to the same relationships as bottom
      (loop with target-top-id = (id target-top)
            with target-bottom-id = (id target-bottom)
            for object in (remove target-top (objects target-set))
            for new-relationships
            = (loop for (relation . object-ids) in (relationships object)
                    if (and (find target-top-id object-ids)
                            (find target-bottom-id object-ids))
                    collect (cons relation object-ids)
                    else if (find target-top-id object-ids)
                    collect (cons relation (remove target-top-id object-ids))
                    else if (find target-bottom-id object-ids)
                    collect (cons relation (cons target-top-id object-ids))
                    else collect (cons relation object-ids))
            do (setf (relationships object) new-relationships))
      ;; top-object can be the only one on top of bottom-object
      (set-ids-as-object-relations
       target-bottom 'on
       (list (id target-top)))
      ;; bottom-object is below top-object
      (set-ids-as-object-relations
       target-top 'below
       (list (id target-bottom)))
      ;; top gets all the same left/right/front/back as bottom
      (loop for relation in '(left right front back)
            for bottom-rel = (get-ids-from-object-relations target-bottom relation)
            do (set-ids-as-object-relations target-top relation bottom-rel))
      ;; return the target set
      target-set)))
      
    
;; stack some object on top op some other object
;; because this primitive changes attributes of objects
;; it gives a changed set of objects as output
(defprimitive stack ((target-set simple-object-set)
                     (bottom-object simple-object)
                     (top-object simple-object))
  ((bottom-object top-object => target-set)
   (let* ((context (get-data ontology 'context))
          (computed-set (stack-objects context bottom-object top-object)))
     (when computed-set
       (bind (target-set 1.0 computed-set)))))

  ((bottom-object => top-object target-set)
   ;; compute all possible objects that can go on top of bottom-object
   (let ((context (get-data ontology 'context)))
     (loop for candidate-object in (objects context)
           for computed-set = (stack-objects context bottom-object candidate-object)
           when computed-set
           do (bind (top-object 1.0 candidate-object)
                    (target-set 1.0 computed-set)))))

  ((top-object => bottom-object target-set)
   ;; compute all possible objects that top-object can be put on
   (let ((context (get-data ontology 'context)))
     (loop for candidate-object in (objects context)
           for computed-set = (stack-objects context candidate-object top-object)
           when computed-set
           do (bind (bottom-object 1.0 candidate-object)
                    (target-set 1.0 computed-set)))))

  ((target-set => bottom-object top-object)
   ;; find out from context and target-set which object
   ;; has been stacked on which other object
   )

  ((bottom-object top-object target-set =>)
   ;; check the source-set, bottom-object, top-object and target-set
   ;; and see if the bindings are consistent (i.e. are the objects
   ;; effectively stacked correctly in the target set?)
   )
  :primitive-inventory *simple-primitives*)

;; move
;; ----

(defun get-inverse-relation (relation)
  (case relation
    (left 'right) (right 'left)
    (front 'back) (back 'front)))

(defun move-object (source-set moveable-object reference-object spatial-relation)
  ;; first, check constraints
  (unless (or (equal-entity moveable-object reference-object)
              ;; moveable-object is not moveable when it has something on top
              (get-ids-from-object-relations moveable-object 'on))
    ;; if ok, make the move!
    ;; example: moving OBJ-3 left of OBJ-2
    ;; for each object, check its relation to OBJ-2
    ;; if my relation to OBJ-2 is X, OBJ-3 also becomes X
    ;; X depends on the axis of spatial relation; left/right or front/back
    (let* ((target-set (copy-object source-set))
           (target-moveable (find-entity-by-id target-set (id moveable-object)))
           (target-reference (find-entity-by-id target-set (id reference-object)))
           (relations-to-check
            (case (category spatial-relation)
              (left '(left right)) (right '(left right))
              (front '(front back)) (back '(front back)))))
      ;; change all objects except moveable and reference
      ;; for each object, remove moveable and add it to
      ;; the same relation as the reference object
      (loop for object in (set-difference (objects target-set)
                                          (list target-moveable target-reference))
            for new-relationships
            = (loop for (relation . object-ids) in (relationships object)
                    if (find relation relations-to-check)
                    collect (cond ((and (find (id target-moveable) object-ids)
                                        (find (id target-reference) object-ids))
                                   (cons relation object-ids))
                                  ((find (id target-moveable) object-ids)
                                   (cons relation (remove (id target-moveable) object-ids)))
                                  ((find (id target-reference) object-ids)
                                   (cons relation (cons (id target-moveable) object-ids)))
                                  (t (cons relation object-ids)))
                    else collect (cons relation object-ids))
            do (setf (relationships object) new-relationships))     
      ;; change relations of reference:
      ;; add moveable to spatial-relation
      ;; and remove it from somewhere else
      (loop for (relation . object-ids) in (relationships target-reference)
            when (and (find relation relations-to-check)
                      (find (id target-moveable) object-ids))
            do (remove-ids-from-object-relations
                target-reference relation
                (list (id target-moveable))))
      (push-ids-to-object-relations
       target-reference (category spatial-relation)
       (list (id target-moveable)))
      ;; change relations of moveable
      ;; it has all the same relations as reference
      ;; but remove itself from those
      ;; and add reference in the inverse relation
      (setf (relationships target-moveable)
            (copy-object (relationships target-reference)))
      (loop for (relation . object-ids) in (relationships target-moveable)
            when (and (find relation relations-to-check)
                      (find (id target-moveable) object-ids))
            do (remove-ids-from-object-relations
                target-moveable relation
                (list (id target-moveable))))
      (push-ids-to-object-relations
       target-moveable
       (get-inverse-relation (category spatial-relation))
       (list (id target-reference)))
      ;; return the target set
      target-set)))
       
                              
            
      

;; move the moveable-object such that it has spatial-relation
;; with respect to the reference-object
;; only the axis of spatial-relation is changed (e.g. only left/right)
;; while the other axis (front/back) remains the same
;; I also assume that the movable object is moved closest to the reference object

;; For example, in the following scene:
;; obj-1 obj-2 obj-3
;; when moving obj-3 left of obj-2
;; the result is
;; obj-1 obj-3 obj-2
;; and NOT:
;; obj-3 obj-1 obj-2
(defprimitive move ((target-set simple-object-set)
                    (moveable-object simple-object)
                    (reference-object simple-object)
                    (spatial-relation spatial-relation))
  ((moveable-object reference-object spatial-relation => target-set)
   (let* ((context (get-data ontology 'context))
          (computed-set (move-object context moveable-object reference-object spatial-relation)))
     (when computed-set
       (bind (target-set 1.0 computed-set)))))

  ((moveable-object reference-object => target-set spatial-relation)
   ;; compute all possiblities for moving the object w.r.t. reference object
   (let* ((context (get-data ontology 'context)))
     (loop for relation in (get-data ontology 'spatial-relations)
           for computed-set = (move-object context moveable-object reference-object relation)
           when computed-set
           do (bind (target-set 1.0 computed-set)
                    (spatial-relation 1.0 relation)))))

  ((moveable-object spatial-relation => target-set reference-object)
   ;; compute all possibilities for moving the object using spatial relation
   )

  ((reference-object spatial-relation => target-set moveable-object)
   )

  ((moveable-object => target-set reference-object spatial-relation)
   ;; compute all possibilities for moving the moveable-object
   )

  ((reference-object => target-set moveable-object spatial-relation)
   )

  ((target-set => moveable-object reference-object spatial-relation)
   ;; find out from the target set and the context
   ;; which object has been moved with respect to which other object
   ;; and in what spatial relation
   )

  ((moveable-object reference-object target-set spatial-relation =>)
   ;; compute consistency
   )
  :primitive-inventory *simple-primitives*)

;; To integrate stack and move with the other primitives
;; (and not just use it as the final primitive)
;; it might be necessary to add an input-set and output-set
;; argument to all primitives.


;; Part 3: Evaluating programs
;; ---------------------------

(defparameter *simple-ontology* (build-initial-ontology))
(make-example-context *simple-ontology*)

(add-element (make-html *simple-ontology*))

(activate-monitor trace-irl)

;; are there any tiny cubes?
(evaluate-irl-program
 '((observe-objects ?objects)
   (filter ?set-1 ?objects ?shape-1)
   (filter ?set-2 ?set-1 ?size-1)
   (exist ?target ?set-2)

   (bind shape-category ?shape-1 cube)
   (bind size-category ?size-1 tiny))
 *simple-ontology*
 :primitive-inventory
 *simple-primitives*)

;; how many green balls are there?
(evaluate-irl-program
 '((observe-objects ?objects)
   (filter ?set-1 ?objects ?shape-1)
   (filter ?set-2 ?set-1 ?color-1)
   (count-set ?target ?set-2)

   (bind shape-category ?shape-1 ball)
   (bind color-category ?color-1 green))
 *simple-ontology*
 :primitive-inventory
 *simple-primitives*)

;; what color is the small pyramid?
(evaluate-irl-program
 '((observe-objects ?objects)
   (filter ?set-1 ?objects ?shape-1)
   (filter ?set-2 ?set-1 ?size-1)
   (unique ?obj-1 ?set-2)
   (query ?target ?obj-1 ?attr-1)

   (bind shape-category ?shape-1 pyramid)
   (bind size-category ?size-1 small)
   (bind attribute-category ?attr-1 color))
 *simple-ontology*
 :primitive-inventory
 *simple-primitives*)

;; are there any pyramids left of the large cube
(evaluate-irl-program
 '((observe-objects ?objects)
   (filter ?set-1 ?objects ?shape-1)
   (filter ?set-2 ?set-1 ?size-1)
   (unique ?obj-1 ?set-2)
   (relate ?set-3 ?obj-1 ?relation-1)
   (filter ?set-4 ?set-3 ?shape-2)
   (exist ?target ?set-4)

   (bind shape-category ?shape-1 cube)
   (bind size-category ?size-1 large)
   (bind spatial-relation ?relation-1 left)
   (bind shape-category ?shape-2 pyramid))
 *simple-ontology*
 :primitive-inventory
 *simple-primitives*)

;; put the tiny cube on top of the large cube
(evaluate-irl-program
 '((observe-objects ?objects)
   (filter ?set-1 ?objects ?shape-1)
   (filter ?set-2 ?set-1 ?size-1)
   (unique ?obj-1 ?set-2)

   (filter ?set-3 ?objects ?shape-2)
   (filter ?set-4 ?set-3 ?size-2)
   (unique ?obj-2 ?set-4)

   (stack ?target ?obj-1 ?obj-2)

   (bind shape-category ?shape-1 cube)
   (bind size-category ?size-1 large)
   (bind shape-category ?shape-2 cube)
   (bind size-category ?size-2 tiny))
 *simple-ontology*
 :primitive-inventory
 *simple-primitives*)


;;; Try the output of the SHRDLU grammar here below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defparameter *simple-ontology* (build-initial-ontology))
;(make-example-context *simple-ontology*)


