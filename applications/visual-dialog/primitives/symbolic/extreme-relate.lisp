(in-package :visual-dialog)

;; -----------------
;; EXIST-OR-COUNT primitive 
;; -----------------

(defprimitive extreme-relate ((target-set world-model)
                              (source-set world-model)
                              (scene pathname-entity)
                              (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-set spatial-relation scene => target-set)
   (let* ((relation-list nil)
          (id-right nil)
          (object-right nil))
     (cond ((eql (spatial-relation spatial-relation) 'right)
            (setf id-extreme-object (rightmost (scene-configuration (object-set (first (set-items source-set)))))))
           ((eql (spatial-relation spatial-relation) 'left)
            (setf id-extreme-object (leftmost (scene-configuration (object-set (first (set-items source-set)))))))
           ((eql (spatial-relation spatial-relation) 'front)
            (setf id-extreme-object (most-in-front (scene-configuration (object-set (first (set-items source-set)))))))
           ((eql (spatial-relation spatial-relation) 'behind)
            (setf id-extreme-object (most-in-back (scene-configuration (object-set (first (set-items source-set)))))))
           ((eql (spatial-relation spatial-relation) 'center) (setf id-extreme-object (middle (scene-configuration (object-set (first (set-items source-set))))))))
     
     (if (listp id-extreme-object)
       (let ((objects-list nil))
         (loop for id in id-extreme-object
               do (loop for object in (objects (object-set (first (set-items source-set))))
                        do (if (equal (id object) id)
                             (push object objects-list))))
         (bind (target-set 1.0 (make-instance 'world-model :set-items (list (make-instance 'turn :object-set (make-instance 'object-set :objects objects-list)))))))
     
     #|(if (NOT (listp id-extreme-object))|#
       (let ((object-right nil))
         (loop for object in (objects (object-set (first (set-items source-set))))
               do (progn
                    ;(print (id object))
                    (if (equal (id object) id-extreme-object)
                      (setf object-right object))))
         (if object-right
           (bind (target-set 1.0 (make-instance 'world-model :set-items (list (make-instance 'turn :object-set (make-instance 'object-set :objects (list object-right))))))))))))
  :primitive-inventory *symbolic-primitives*)

