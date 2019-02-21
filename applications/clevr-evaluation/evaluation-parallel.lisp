;;;; evaluation-parallel.lisp

(in-package :clevr-evaluation)

(defun evaluate-parallel-entry (question-entry &key timeout ontology)
  "Evaluate a question entry. This consists of doing comprehension,
   comparing the irl-program to the clevr functional program,
   evaluating the irl-program and comparing the found answer to
   the ground truth answer."
  (let* ((question (rest (assoc :question question-entry)))
         (question-index (rest (assoc :question--index question-entry)))
         (functional-program (rest (assoc :program question-entry)))
         (ground-truth-answer (rest (assoc :answer question-entry)))
         (output-entry (remove :program question-entry :key #'car))
         fcg-status
         processed-irl-program
         compare-program-success
         equal-answer-success)
    ;; run comprehension
    (multiple-value-bind (irl-program cipn) (clevr-comprehend-with-timeout question timeout :silent t)
      (when irl-program
        (setf fcg-status (first (statuses cipn)))
        (setf processed-irl-program (preprocess-program irl-program)))
      (if (eql 'FCG::SUCCEEDED fcg-status)
        (format t "~%[~a]:Comprehension: yes!~%" question-index)
        (format t "~%[~a]:Comprehension: no!~%" question-index))
      (push (cons :comprehension--success (eql 'FCG::SUCCEEDED fcg-status)) output-entry)
      ;; compare functional programs
      (when processed-irl-program
        (multiple-value-bind (rpn fcg-program-tree) (program->rpn+tree processed-irl-program)
          (let ((clevr-program-tree (when functional-program (json->program-tree functional-program))))
            (setf compare-program-success
                  (and fcg-program-tree clevr-program-tree
                       (equal-program-tree clevr-program-tree fcg-program-tree)))
            (if compare-program-success
              (format t "~%[~a]:Equal Programs: yes!~%" question-index)
              (format t "~%[~a]:Equal Programs: no!~%" question-index))
            (push (cons :meaning rpn) output-entry)
            (push (cons :equal--programs compare-program-success) output-entry))))
      ;; run IRL
      (let ((solutions (evaluate-irl-program irl-program ontology)))
        (setf equal-answer-success
              (compare-answers (first solutions)
                               irl-program
                               ground-truth-answer
                               question-index))
        (if equal-answer-success
          (format t "~%[~a]:Correct Answer: yes!~%" question-index)
          (format t "~%[~a]:Correct Answer: no!~%" question-index))
        (push (cons :equal--answers equal-answer-success) output-entry)))
    output-entry))

(defun process-parallel-entry (scene-entry &key timeout questions-dir)
  "Process a single scene entry. This consists of reading the questions
   and processing each of them in this scene. Returns a list of json objects."
  (let* ((image-index (rest (assoc :image--index scene-entry)))
         (context (process-json-context scene-entry))
         (questions-file (merge-pathnames
                          (make-pathname :name (format nil "CLEVR_questions_scene_~a_per_line" image-index)
                                         :type "json")
                          questions-dir))
         (questions (with-open-file (stream questions-file :direction :input)
                      (mapcar #'decode-json-from-string (stream->list stream))))
         (ontology (copy-object *clevr-ontology*)))
    (set-data ontology 'clevr-context context)
    (loop for question-entry in questions
          collect (evaluate-parallel-entry question-entry
                                           :timeout timeout
                                           :ontology ontology))))

#+LISPWORKS
(defun evaluate-clevr-in-parallel (input-file output-file questions-dir)
  "Wrapper around json-stream-process function for evaluating
   the CLEVR Grammar with IRL in parallel and keeping three accuracy scores"
  (let (comprehension-success
        equal-programs
        equal-answers)
    (json-stream-process :function #'process-parallel-entry
                         :function-kwargs `(:timeout 60 :questions-dir ,questions-dir)
                         :input-file input-file
                         :output-file output-file
                         :output-format :json
                         :process-batch-fn
                         #'(lambda (list-of-results)
                             (mapcar #'(lambda (json-obj)
                                         (push (if (rest (assoc :comprehension--success json-obj)) 1 0) comprehension-success)
                                         (push (if (rest (assoc :equal--programs json-obj)) 1 0) equal-programs)
                                         (push (if (rest (assoc :equal--answers json-obj)) 1 0) equal-answers))
                                     list-of-results)
                             list-of-results)
                         :number-of-threads 4
                         :number-of-objects-per-thread 1)
    (values
     (average (substitute 0 nil comprehension-success))
     (average (substitute 0 nil equal-programs))
     (average (substitute 0 nil equal-answers)))))