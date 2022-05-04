(in-package :visual-dialog)

(defun understand-execute-remember-caption (scene-pathname input-sentence ontology &key (silent t))
  "first comprehend input, then execute, then update memory, handles the processing of the caption and starts specific functions based on the type of the captions that make history"
  (multiple-value-bind (irl-program cipn)
      (clevr-dialog-grammar::understand-until-solution input-sentence :silent (if silent silent))
    (let* ((scene-var (extract-scene-unit-variable cipn))
           (scene-var-bind-statement `(bind pathname-entity ,scene-var ,(make-instance 'pathname-entity :path scene-pathname)))
           (irl-program (append (list scene-var-bind-statement) irl-program))
           (solutions (evaluate-irl-program irl-program ontology :silent (if silent silent) :primitive-inventory (get-primitive-inventory (get-data ontology 'world)))))
      (if solutions
        (let* ((list-of-bindings (first solutions))
               memory)
          (cond ((find 'immediate-relate irl-program :test #'equal :key #'first)
                 (setf memory (initialize-memory-object-relation-caption input-sentence irl-program list-of-bindings)))
                ((find 'extreme-relate irl-program :test #'equal :key #'first)
                 (setf memory (initialize-memory-extreme-relation-caption input-sentence irl-program list-of-bindings)))
                ((or (find 'clevr-dialog-grammar::select-one irl-program :test #'equal :key #'first)
                     (find 'unique irl-program :test #'equal :key #'first))
                 (setf memory (initialize-memory-unique-object-caption input-sentence irl-program list-of-bindings)))
                ((or (find 'count-objects irl-program :test #'equal :key #'first)
                     (find 'more-than-1 irl-program :test #'equal :key #'first))
                 (setf memory (initialize-memory-multiple-objects-caption input-sentence irl-program list-of-bindings))))
          (when (not silent)
            (add-conversation-memory memory))
          (set-data ontology 'conversation-memory memory)
          memory)
        (make-instance 'world-model
                     :id 'conversation-memory
                     :set-items (list (make-instance 'turn
                                                     :timestamp 1)))))))

(defun understand-execute-remember-first-question (scene-pathname input-sentence ontology &key (silent t))
  "starts conversation memory when no caption"
  (multiple-value-bind (irl-program cipn)
      (clevr-dialog-grammar::understand-until-solution input-sentence :silent (if silent silent))
  (let* ((scene-var (extract-scene-unit-variable cipn))
         (scene-var-bind-statement `(bind pathname-entity ,scene-var ,(make-instance 'pathname-entity :path scene-pathname)))
         (irl-program (push scene-var-bind-statement irl-program))
         (solutions (evaluate-irl-program irl-program ontology :silent (if silent silent) :primitive-inventory (get-primitive-inventory (get-data ontology 'world)))))
    (if solutions
      (let* ((target-value (get-target-value irl-program (first solutions)))
             (source-value (get-third-value-target-primitive irl-program (first solutions)))
             (target-primitive (get-target-primitive irl-program))
             (new-object-set (make-instance 'object-set :id (make-id 'set)))
             question memory last-set)
        (if (eq target-primitive 'QUERY)
          (progn
            (setf question 'query)
            (update-memory-query irl-program solutions source-value target-value new-object-set)))
        (if (eq target-primitive 'count-objects)
          (progn
            (setf question 'count)
            (update-memory-count-or-exist irl-program target-primitive source-value new-object-set solutions)))
        (setf memory (make-instance 'world-model
                                    :id 'conversation-memory
                                    :set-items (list (make-instance 'turn
                                                                    :timestamp 1
                                                                    :question-type question
                                                                    :question input-sentence
                                                                    :answer (id target-value)
                                                                    :topic-list (find-topic source-value)
                                                                    :object-set new-object-set))))
        (when (not silent)
          (add-conversation-memory memory))
        (set-data ontology 'conversation-memory memory)
        memory)
      (make-instance 'world-model
                     :id 'conversation-memory
                     :set-items (list (make-instance 'turn
                                                     :timestamp 1)))))))
  

(defun understand-execute-remember (scene-pathname input-sentence memory ontology &key (silent t))
  "returns answer to question and updates memory"
  (multiple-value-bind (last-set last-timestamp)
      (the-biggest #'timestamp (set-items memory))
        (multiple-value-bind (irl-program cipn)
            (clevr-dialog-grammar::understand-until-solution input-sentence :silent (if silent silent))
          (let* ((scene-var (extract-scene-unit-variable cipn))
                 (scene-var-bind-statement `(bind pathname-entity ,scene-var ,(make-instance 'pathname-entity :path scene-pathname)))
                 (irl-program (push scene-var-bind-statement irl-program))
                 (memory-var (extract-memory-unit-variable cipn))
                 (memory-var-bind-statement `(bind world-model ,memory-var ,memory))
                 (irl-program (push memory-var-bind-statement irl-program))
                 (solutions (evaluate-irl-program irl-program ontology :silent (if silent silent) :primitive-inventory (get-primitive-inventory (get-data ontology 'world)))))
            (if solutions
              (let* ((target-value (get-target-value irl-program (first solutions)))
                     (source-value (get-third-value-target-primitive irl-program (first solutions)))
                     (target-primitive (get-target-primitive irl-program))
                     (new-object-set
                      (if (object-set last-set)
                        (make-instance 'object-set
                                       :objects (loop for object in (objects (object-set last-set))
                                                      collect (copy-object object)))
                        (make-instance 'object-set)
; :scene-configuration (copy-scene-configuration (object-set last-set))
                                                   ))
                    items-list question)
                (if (not (eq target-primitive 'exist-or-count))
                  (setf question target-primitive)
                  (setf question (question-type last-set)))
                (cond ((eq target-primitive 'QUERY)
                       (update-memory-query irl-program solutions source-value target-value new-object-set))
                      ((eq target-primitive 'count-objects)
                       (update-memory-count-or-exist irl-program target-primitive source-value new-object-set solutions ))
                      ((eq target-primitive 'EXIST)
                       (update-memory-count-or-exist irl-program target-primitive source-value new-object-set solutions ))
                      ((eq target-primitive 'EXIST-OR-COUNT)
                       (if (equal (question-type last-set) 'exist)
                         (update-memory-count-or-exist irl-program target-primitive source-value new-object-set solutions ))
                       (if (equal (question-type last-set) 'count-objects)
                         (update-memory-count-or-exist irl-program target-primitive source-value new-object-set solutions ))))
                (setf new-item (make-instance 'turn
                                              :timestamp (+ last-timestamp 1)
                                              :object-set new-object-set
                                              :question-type question
                                              :question input-sentence
                                              :answer (id target-value)
                                              :topic-list (find-topic source-value)))
                (push new-item (set-items memory))
                (when (not silent)
                  (add-conversation-memory memory))
                (id target-value))
              nil)))))




