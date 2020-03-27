;;;; evaluation.lisp

(in-package :clevr-evaluation)

(defun clevr-comprehend-with-timeout (question timeout &key (silent t))
  (multiple-value-bind (irl-program cipn)
      (handler-case (trivial-timeout:with-timeout (timeout)
                      (fcg:comprehend (preprocess-sentence question)
                                      :cxn-inventory *clevr*
                                      :silent silent))
        (trivial-timeout:timeout-error (error)
          (values nil nil)))
    (values irl-program cipn)))

(define-event question-entry-compare-answers
  (irl-answer string) (ground-truth-answer string))

(defun compare-answers (list-of-bindings irl-program ground-truth-answer)
  "Get the target variable from the irl-program and look up its value
   in the list of bindings. Compare this to the ground truth answer."
  (let ((target-value (get-target-value irl-program list-of-bindings)))
    (notify question-entry-compare-answers (answer->str target-value) ground-truth-answer)
    (when target-value
      (string= (upcase ground-truth-answer)
               (upcase (answer->str target-value))))))

(define-event question-entry-evaluation-started (image-filename string)
  (split string) (question string) (question-index number))
(define-event question-entry-evaluation-finished (success t))

(defun evaluate-question (scene q &key (comprehension-timeout 60))
  (declare (ignorable comprehension-timeout))
  (multiple-value-bind (irl-program cipn)
      (comprehend (preprocess-sentence (question q)) :cxn-inventory *clevr* :silent t)
    (let ((solutions (evaluate-irl-program irl-program :primitive-inventory *clevr-primitives*)))
      (values 1 1 1))))

#|
(defun evaluate-question (scene q &key (comprehension-timeout 60))
  (let ((comprehension-success 0)
        (equal-program-success 0)
        (equal-answer-success 0)
        fcg-status processed-irl-network)
    (notify question-entry-evaluation-started
            (pathname-name (image scene))
            (data-set scene) (question q) (index q))
    ;; comprehend
    (multiple-value-bind (irl-program cipn)
        (clevr-comprehend-with-timeout (question q) comprehension-timeout :silent nil)
      (when irl-program
        (setf fcg-status (first (statuses cipn)))
        (setf processed-irl-network (preprocess-program irl-program)))
      (when (eql 'FCG::SUCCEEDED fcg-status)
        (setf comprehension-success 1))
      ;; compare programs
      (when processed-irl-network
        (let* ((fcg-program (program->program-tree processed-irl-network))
               (clevr-program (program q))
               (compare-program-success (and fcg-program clevr-program
                                             (equal-program-tree clevr-program fcg-program))))
          (when compare-program-success
            (setf equal-program-success 1))))
      ;; compare answers
      (let* ((solutions (evaluate-irl-program irl-program :primitive-inventory *clevr-primitives*))
             (equal-answer (when solutions
                             (compare-answers (first solutions) irl-program (answer q)))))
        (when equal-answer
          (setf equal-answer-success 1))))
    (notify question-entry-evaluation-finished (= equal-answer-success 1))
    (values comprehension-success
            equal-program-success
            equal-answer-success)))
|#

(defun evaluate-scene (scene question-set
                             &key (comprehension-timeout 60)
                             nr-of-questions
                             wait-per-question)
  ;; choose some questions
  (let ((questions (if nr-of-questions
                     (random-elts (questions question-set) nr-of-questions)
                     (questions question-set)))
        comprehension-success
        equal-program-success
        equal-answer-success)
    ;; evaluate each question and store the results
    (loop for q in questions
          do (multiple-value-bind (c-success ep-success ea-success)
                 (evaluate-question scene q :comprehension-timeout comprehension-timeout)
               (push c-success comprehension-success)
               (push ep-success equal-program-success)
               (push ea-success equal-answer-success))
          do (when wait-per-question
               (break "Ready to go to the next question?")))
    ;; return the results
    (values comprehension-success
            equal-program-success
            equal-answer-success)))

(define-event clevr-evaluation-started)
(define-event clevr-evaluation-finished (comprehension-success number)
  (equal-program-success number) (equal-answer-success number))

(defun evaluate-clevr (clevr-world
                       &key (comprehension-timeout 60)
                       nr-of-contexts
                       nr-of-questions
                       wait-per-question)
  "Evaluate clevr sequentially"
  ;; define the success measures
  (let (comprehension-success
        equal-program-success
        equal-answer-success)
    (notify clevr-evaluation-started)
    ;; get the nr of contexts
    (let ((total-nr-of-contexts (length (scenes clevr-world)))
          (scenes-evaluated 0))
      ;; go over scenes and questions
      (do-for-scenes-and-questions clevr-world
         (lambda (scene question-set)
           (when (< scenes-evaluated (if nr-of-contexts nr-of-contexts total-nr-of-contexts))
             (incf scenes-evaluated)
             ;; set the scene as 'clevr-context
             (set-data *clevr-ontology* 'clevr-context scene)
             ;; evaluate the scene
             (multiple-value-bind (c-success ep-success ea-success)
                 (evaluate-scene scene question-set
                                 :comprehension-timeout comprehension-timeout
                                 :nr-of-questions nr-of-questions
                                 :wait-per-question wait-per-question)
               ;; append the results
               (setf comprehension-success (append c-success comprehension-success))
               (setf equal-program-success (append ep-success equal-program-success))
               (setf equal-answer-success (append ea-success equal-answer-success))
               t)))
         :shuffled t)
      ;; notify and return the results
      (notify clevr-evaluation-finished
              (average comprehension-success)
              (average equal-program-success)
              (average equal-answer-success))
      (values (average comprehension-success)
              (average equal-program-success)
              (average equal-answer-success)))))