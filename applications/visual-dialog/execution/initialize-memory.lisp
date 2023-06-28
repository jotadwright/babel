(in-package :visual-dialog)

(defun initialize-memory-object-relation-caption (input-sentence irl-program list-of-bindings)
  "memory is initialized based on caption type object relation, information that is stored: attributes of two unique inputs + relation between them"
  (let* ((relate-primitive (find 'immediate-relate irl-program :test #'equal :key #'first))     
         (first-unique-variable (third relate-primitive))
         (first-unique-binding (first (objects (object-set (first (set-items (value (find first-unique-variable list-of-bindings :key #'var))))))))
         (first-unique-attention (if (attention first-unique-binding)
                                   (attention first-unique-binding)))
          
         (exist-primitive (find 'exist irl-program :test #'equal :key #'first))
         (second-unique-variable (third exist-primitive))
         (second-unique-binding (first (objects (object-set (first (set-items (value (find second-unique-variable list-of-bindings :key #'var))))))))
         (second-unique-attention (if (attention second-unique-binding)
                                   (attention second-unique-binding)))
         
         (attribute-first-unique-variable-list (find-attributes-of-unique irl-program first-unique-variable))
         (attribute-second-unique-variable-list (find-attributes-of-unique irl-program second-unique-variable))
         (relation-variable (last-elt relate-primitive))
         (relation-binding (spatial-relation (value (find relation-variable list-of-bindings :key #'var))))
         new-object-list new-world-model left-right-relations front-behind-relations)
    
    (push (make-new-object-with-attributes (id second-unique-binding) attribute-second-unique-variable-list second-unique-attention) new-object-list)
    (push (make-new-object-with-attributes (id first-unique-binding) attribute-first-unique-variable-list first-unique-attention) new-object-list)

    (setf new-world-modelz
          (make-instance 'world-model
                         :id 'conversation-memory
                         :set-items (list
                                     (make-instance 'turn
                                                    :timestamp 1
                                                    :question-type 'caption
                                                    :question input-sentence
                                                    :topic-list (list (id second-unique-binding) (id first-unique-binding))
                                                    :object-set (make-instance 'object-set
                                                                               :id (make-id 'set)
                                                                               :objects new-object-list
                                                                               :scene-configuration (make-instance 'relation-set
                                                                                                                   :immediate-right (if left-right-relations left-right-relations (make-var 'immediate-right-list))
                                                                                                                   :immediate-front (if front-behind-relations front-behind-relations (make-var 'immediate-front-list))))))))
    new-world-model))

(defun initialize-memory-unique-object-caption (input-sentence irl-program list-of-bindings)
  "memory is initialized based on caption type unique object, information that is stored: attributes of unique object"
  (let* ((unique-primitive (find 'unique irl-program :test #'equal :key #'first))
         (exist-primitive (find 'exist irl-program :test #'equal :key #'first))
         (unique-object (third exist-primitive))
         (unique-object-binding (first (objects (object-set (first (set-items (value (find unique-object list-of-bindings :key #'var))))))))
         (unique-attention (if (attention unique-object-binding)
                                   (attention unique-object-binding)))
         
         (attribute-unique-variable-list (find-attributes-of-unique irl-program unique-object))
         new-object-list new-world-model)
    (setf new-object-list (list (make-new-object-with-attributes (id unique-object-binding) attribute-unique-variable-list unique-attention)))
    (setf new-world-model (make-instance 'world-model
                                         :id 'conversation-memory
                                         :set-items (list (make-instance 'turn
                                                                         :timestamp 1
                                                                         :question-type 'caption
                                                                         :question input-sentence
                                                                         :topic-list (list (id (first new-object-list)))
                                                                         :object-set (make-instance 'object-set
                                                                                                    :id (make-id 'set)
                                                                                                    :objects new-object-list)))))
    new-world-model))



(defun initialize-memory-multiple-objects-caption (input-sentence irl-program list-of-bindings)
  "history is made based on caption type multiple objects, information that is stored: attributes of objects"
  (let* ((multiple-primitive (or (find 'clevr-dialog-grammar::count-objects irl-program :test #'equal :key #'first)
                                 (find 'clevr-dialog-grammar::more-than-1 irl-program :test #'equal :key #'first)))
         (target-variable (second multiple-primitive))
         (source-variable (third multiple-primitive))
         (source-value (value (find source-variable list-of-bindings :key #'var)))
         (attribute-variable-list (find-attributes-of-unique irl-program target-variable))
         
         new-object-list new-world-model)
    (setf new-object-list
          (loop for object in (objects (object-set (first (set-items source-value))))
                collect (make-new-object-without-attributes (id object) attribute-variable-list (attention object))))
    ;(setf new-object-list (find-attributes-of-several-object attribute-variable-list context-binding new-object-list list-of-bindings))
    (setf new-world-model (make-instance 'world-model :id 'conversation-memory
                                         :set-items (list (make-instance 'turn
                                                                         :timestamp 1
                                                                         :question-type 'caption
                                                                         :question input-sentence
                                                                         :topic-list (loop for obj in new-object-list
                                                                                           collect (id obj))
                                                                         :object-set (make-instance 'object-set
                                                                                                    :id (make-id 'set)
                                                                                                    :objects new-object-list)))))
    new-world-model))

(defun initialize-memory-extreme-relation-caption (input-sentence irl-program list-of-bindings)
  "history is made based on caption type extreme relation, information that is stored: attributes of unique object + extreme relation"
  (let* ((extreme-relate-primitive (find 'extreme-relate irl-program :test #'equal :key #'first))
         
         (exist-primitive (find 'exist irl-program :test #'equal :key #'first))

         (extreme-object-variable (third exist-primitive))
         (extreme-object-binding (first (objects (object-set (first (set-items (value (find extreme-object-variable list-of-bindings :key #'var))))))))
         (extreme-attention (if (attention extreme-object-binding)
                                   (attention extreme-object-binding)))
         

         (relation-variable (last-elt extreme-relate-primitive))
         (relation-binding (spatial-relation (value (find relation-variable list-of-bindings :key #'var))))

         (attributes (find-attributes-of-unique irl-program extreme-object-variable))
         new-object-list)
    (setf new-object-list (list (make-new-object-with-attributes (id extreme-object-binding) attributes extreme-attention)))
    (setf new-world-model (make-instance 'world-model
                                         :id 'conversation-memory
                                         :set-items (list (make-instance 'turn
                                                                         :timestamp 1
                                                                         :question-type 'caption
                                                                         :question input-sentence
                                                                         :topic-list (list (id (first new-object-list)))
                                                                         :object-set (make-instance 'object-set
                                                                                                    :id (make-id 'set)
                                                                                                    :objects new-object-list)))))))


