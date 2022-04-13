(in-package :visual-dialog)

;; ------------------
;; IMMEDIATE RELATE primitive ;;
;; ------------------

(defprimitive immediate-relate ((target-set world-model)
                                (source-world-model world-model)
                                (segmented-scene world-model)
                                (scene pathname-entity)
                                (spatial-relation category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-world-model spatial-relation segmented-scene scene => target-set)
   (let* (;(context (get-data ontology 'context))
          ;(context (cdr (find (pathname scene) (get-data ontology 'segmented-scene) :test #'equal :key #'first)))
          (context segmented-scene)
          (source-object (first (objects (object-set (first (set-items source-world-model))))))
          relation-list id-right object-right)
     (if (eq (type-of spatial-relation) 'spatial-relation-category)
       (cond ((eql (spatial-relation spatial-relation) 'right)
              (setf relation-list (immediate-right (scene-configuration (object-set (first (set-items context)))))))
             ((eql (spatial-relation spatial-relation) 'left)
              (setf relation-list (loop for item in (reverse (immediate-right (scene-configuration (object-set  (first (set-items context))))))
                                        collect (reverse item))))
             ((eql (spatial-relation spatial-relation) 'front)
              (setf relation-list (immediate-front (scene-configuration (object-set (first (set-items context)))))))
             ((eql (spatial-relation spatial-relation) 'behind)
              (setf relation-list (loop for item in (reverse (immediate-front (scene-configuration (object-set  (first (set-items context))))))
                                        collect (reverse item))))))

     (if (eq (type-of spatial-relation) '2D-relation-category)
       (cond ((eql (2D-relation spatial-relation) '2D-right)
              (setf relation-list (first (immediate-right (scene-configuration (object-set (first (set-items context))))))))
             ((eql (2D-relation spatial-relation) '2D-left)
              (setf relation-list (loop for item in (reverse (first (immediate-right (scene-configuration (object-set  (first (set-items context)))))))
                                        collect (reverse item))))
             ((eql (2D-relation spatial-relation) 'below)
              (setf relation-list (first (immediate-front (scene-configuration (object-set (first (set-items context))))))))
             ((eql (2D-relation spatial-relation) 'above)
              (setf relation-list (loop for item in (reverse (first (immediate-front (scene-configuration (object-set  (first (set-items context)))))))
                                        collect (reverse item))))))
     (if relation-list
       (progn 
         (loop for item in relation-list
               do (progn
                    (if (eq (car item) (id source-object))
                      (setf id-right (cdr item)))))
         (loop for object in (objects (object-set (first (set-items context))))
               do (if (eq (id object) (car id-right))
                    (setf object-right object)))
         (if object-right
           (bind (target-set 1.0 (make-instance 'world-model :set-items (list (make-instance 'turn :object-set (make-instance 'object-set :objects (list object-right)))))))
           (bind (target-set 1.0 (make-instance 'world-model :set-items (list (make-instance 'turn :object-set (make-instance 'object-set :id 'empty-set)))))))))))
  :primitive-inventory *symbolic-primitives*)
     

