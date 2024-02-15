(in-package :visual-dialog)

(defmethod run-dialog ((scene-pathname pathname)
                       (list-of-sentences list)
                       (world world)
                       (ontology blackboard)
                       (type (eql :clevr))
                       &key (silent t))
  "Running a whole dialog, this means understanding, executing and remembering caption, then understanding and executing each question, Returns list of answers"
  (let* ((ontology (initialize-agent-ontology-and-world ontology world silent))
         (caption (first list-of-sentences))
         (questions (rest list-of-sentences))
         (memory (understand-execute-remember-caption scene-pathname caption ontology :silent silent))
         answer-list)
    ;; memory is updated by understand-execute-remember
    (setf answer-list (append answer-list
                              (loop for question in questions
                                    collect (understand-execute-remember scene-pathname question memory ontology :silent silent))))
    answer-list))

(defmethod run-dialog ((scene-pathname pathname)
                       (list-of-sentences list)
                       (world world)
                       (ontology blackboard)
                       (type (eql :gqa))
                       &key (silent t))
  "Running a whole dialog, this means understanding, executing and remembering caption, then understanding and executing each question, Returns list of answers"
  (let* ((ontology (initialize-agent-ontology-and-world ontology world silent))
         (caption (first list-of-sentences))
         (questions (rest list-of-sentences))
         (memory (understand-execute-remember-caption scene-pathname caption ontology :silent silent))
         answer-list)
    ;; memory is updated by understand-execute-remember
    (setf answer-list (append answer-list
                              (loop for question in questions
                                    collect (understand-execute-remember scene-pathname question memory ontology :silent silent))))
    answer-list))

(defmethod run-dialog ((scene-pathname pathname)
                       (list-of-sentences list)
                       (world world)
                       (ontology blackboard)
                       (type (eql :mnist))
                       &key (silent t))
  "Running a whole dialog, this means understanding, executing and remembering first question in case of mnist, then understanding and executing each question, Returns list of answers"
  (let* ((ontology (initialize-agent-ontology-and-world ontology world silent))
         (caption (first list-of-sentences))
         (questions (rest list-of-sentences))
         (memory (understand-execute-remember-first-question scene-pathname caption ontology :silent silent))
         answer-list)
    ;; memory is updated by understand-execute-remember
    (push (answer (first (set-items memory))) answer-list)
    (setf answer-list (append answer-list
                              (loop for question in questions
                                    collect (understand-execute-remember scene-pathname question memory ontology :silent silent))))
    answer-list))




