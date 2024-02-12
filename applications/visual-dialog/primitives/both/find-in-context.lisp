(in-package :visual-dialog)

(defprimitive find-in-context ((target-set world-model)
                               (context-set world-model)
                               (history-set world-model))
  ((context-set history-set => target-set)
   (multiple-value-bind (last-set last-timestamp) (the-biggest #'timestamp (set-items history-set))
     (let* ((ids (loop for object in (objects (object-set last-set))
                       collect (id object)))
            (object-set nil))
       (loop for object in (objects (object-set (first (set-items context-set))))
             do (loop for id in ids
                      do (if (eq id (id object))
                           (push object object-set))))
       (when object-set
         (bind (target-set 1.0 (make-instance 'world-model
                                              :id 'context
                                              :set-items (list (make-instance 'turn
                                                                              :id (id (first (set-items context-set)))
                                                                              :object-set (make-instance 'object-set :objects object-set)))))))))
  )
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))
   

 