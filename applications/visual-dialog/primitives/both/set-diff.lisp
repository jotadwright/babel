(in-package :visual-dialog)

(defprimitive set-diff ((target-set world-model)
                        (context-set world-model)
                        (history-set world-model)
                        (scene pathname-entity))
  ((context-set history-set scene => target-set)
   (let* ((ids (collect-objects-id-from-world-model history-set))
          (objects (collect-objects-from-world-model context-set))
          (object-set nil))
     (loop for object in objects
           do (if (not (member (id object) ids))
                (push object object-set)))
     (if object-set
       (bind (target-set 1.0 (make-instance 'world-model
                                            :id (id context-set)
                                            :set-items (list (make-instance 'turn
                                                                                         :object-set (make-instance
                                                                                          'object-set
                                                                                          :objects object-set))))))
       (bind (target-set 1.0 (make-instance 'world-model
                                            :id (id context-set)
                                            :set-items (list (make-instance 'turn
                                                                            :object-set (make-instance 'object-set
                                                                                                       :id 'empty-set)))))))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))





