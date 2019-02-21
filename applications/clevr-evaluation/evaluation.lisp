;;;; evaluation.lisp

(in-package :clevr-evaluation)

;; EVALUATION LOOP
;;  1. Load a scene from JSON
;;     --> put this in the ontology with the key 'clevr-context
;;  2. Load all associated questions from JSON
;;     --> use the 'image_index' key to find these
;;     --> store these in a tmp file? Depends how many there are
;;  3. Parse each question using FCG
;;  4. Convert the meaning network and compare against the functional program
;;  5. Execute the meaning on the image metadata using IRL
;;  6. Compare the answer to the ground truth answer

(defun clevr-comprehend-with-timeout (question timeout &key (silent t))
  (multiple-value-bind (irl-program cipn)
      (handler-case (trivial-timeout:with-timeout (timeout)
                      (fcg:comprehend (preprocess-sentence question)
                                      :cxn-inventory *clevr*
                                      :silent silent))
        (trivial-timeout:timeout-error (error)
          (values nil nil)))
    (values irl-program cipn)))

(defun read-questions-from-dir (dir &key image-index)
  "Get the correct file from dir, according to image-index.
   Read and process the file."
  (assert (not (null image-index)))
  (let* ((filename (format nil "CLEVR_questions_scene_~a_per_line" image-index))
         (filepath (merge-pathnames (make-pathname :name filename :type "json") dir)))
    (with-open-file (stream filepath :direction :input)
      (mapcar #'decode-json-from-string
              (stream->list stream)))))

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

(defun evaluate-entry (q-entry timeout)
  "Evaluate a single entry: comprehension, comparing programs,
   irl evaluation and comparing answers."
  ;; default; all success is 0
  (let ((comprehension-success 0)
        (equal-program-success 0)
        (equal-answer-success 0))
    ;; read the question entry
    (let* ((question (rest (assoc :question q-entry)))
           (question-index (rest (assoc :question--index q-entry)))
           (functional-program (rest (assoc :program q-entry)))
           (ground-truth-answer (rest (assoc :answer q-entry)))
           (image-filename (rest (assoc :image--filename q-entry)))
           (split (rest (assoc :split q-entry)))
           fcg-status processed-irl-network)
      (format t "~%[~a]:Comprehending \"~a\"~%" question-index question)
      (notify question-entry-evaluation-started image-filename split question question-index)
      ;; do comprehension with timeout
      (multiple-value-bind (irl-program cipn) (clevr-comprehend-with-timeout question timeout :silent nil)
        ;; preprocess the meaning network for comparison against functional programs
        (when irl-program
          (setf fcg-status (first (statuses cipn)))
          (setf processed-irl-network (preprocess-program irl-program)))
        (if (eql 'FCG::SUCCEEDED fcg-status)
          (progn
            (setf comprehension-success 1)
            (format t "~%[~a]:Comprehension: yes!~%" question-index))
          (format t "~%[~a]:Comprehension: no!~%" question-index))
        (when processed-irl-network
          ;; transform the meaning into a tree and compare
          (multiple-value-bind (rpn fcg-program) (program->rpn+tree processed-irl-network)
            (declare (ignorable rpn))
            (let* ((clevr-program (when functional-program (json->program-tree functional-program)))
                   (compare-program-success (and fcg-program clevr-program
                                                 (equal-program-tree clevr-program fcg-program))))
              (if compare-program-success
                (progn
                  (setf equal-program-success 1)
                  (format t "~%[~a]:Equal Programs: yes!~%" question-index))
                (format t "~%[~a]:Equal Programs: no!~%" question-index)))))
        ;; compute the answer using IRL
        (format t "~%[~a]:Evaluating IRL Progam~%" question-index)
        (let* ((solutions (evaluate-irl-program irl-program *clevr-ontology*))
               (equal-answer (compare-answers (first solutions) irl-program ground-truth-answer)))
          (if equal-answer
            (progn
              (setf equal-answer-success 1)
              (format t "~%[~a]:Correct Answer: yes!~%" question-index))
            (format t "~%[~a]:Correct Answer: no!~%" question-index))
          (notify question-entry-evaluation-finished equal-answer))))
    (values comprehension-success
            equal-program-success
            equal-answer-success)))

(define-event clevr-evaluation-started)
(define-event clevr-evaluation-finished (comprehension-success number)
  (equal-program-success number) (equal-answer-success number))
      
(defun evaluate-clevr (context-file
                       questions-dir
                       &key (comprehension-timeout 60)
                            nr-of-contexts
                            nr-of-questions
                            wait-per-question)
  "Evaluate clevr sequentially"
  ;; define the success measures
  (let (comprehension-success
        equal-program-success
        equal-answer-success)
    (format t "~%Started evaluation.~%~%Reading context from ~a~%~%Reading questions from ~a~%"
            (namestring context-file)
            (namestring questions-dir))
    (notify clevr-evaluation-started)
    ;; get the nr of contexts
    (let ((total-nr-of-contexts (number-of-lines context-file)))
      (format t "~%Evaluating ~a contexts~%"
              (if nr-of-contexts nr-of-contexts total-nr-of-contexts))
      ;; open stream to context-file
      (with-open-file (stream context-file :direction :input)
        ;; read all or nr-of-contexts entries
        (do ((i 0 (1+ i)))
            ((= i (if nr-of-contexts nr-of-contexts total-nr-of-contexts))
             ;; return the average success
             (notify clevr-evaluation-finished
                     (average comprehension-success)
                     (average equal-program-success)
                     (average equal-answer-success))
             (values (average comprehension-success)
                     (average equal-program-success)
                     (average equal-answer-success)))
          ;; read the context from file
          ;; read the corresponding questions
          (format t "~%Reading context ~a from file...~%" i)
          (format t "~%Looking for corresponding questions...~%")
          (let* ((json-context (decode-json-from-string (read-line stream nil nil)))
                 (image-index (rest (assoc :image--index json-context)))
                 (context (process-json-context json-context))
                 (questions (read-questions-from-dir questions-dir :image-index image-index)))
          ;; limit the number of questions per context
          (when nr-of-questions
            (setf questions (random-elts questions nr-of-questions)))
          (format t "~%Evaluating ~a questions on this context~%" (length questions))
          ;; store the context in the ontology
          (set-data *clevr-ontology* 'clevr-context context)
          ;; process the questions on the context
          (dolist (q questions)
            (multiple-value-bind (comp-success ep-success ea-success)
                (evaluate-entry q comprehension-timeout)
              (push comp-success comprehension-success)
              (push ep-success equal-program-success)
              (push ea-success equal-answer-success)
              (when wait-per-question
                (break "Ready to go to the next question?"))))))))))

;; EVALUATE SEQUENTIALLY

#|
(evaluate-clevr-with-irl
 ;; file containing context data
 (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                 :name "CLEVR_val_full_per_line" :type "json")
                  cl-user:*babel-corpora*)
 ;; folder containing question data
 (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "questions" "val"))
                  cl-user:*babel-corpora*)
 ;; mode: evaluate :fcg, :irl or :fcg-irl (default :fcg-irl)
 :mode :fcg-irl
 ;; how many contexts to evaluate (default nil; nil = all)
 :nr-of-contexts 5
 ;; how many questions per context (default nil; nil = all; questions are shuffled)
 :nr-of-questions 2
 ;; wait after every question (for demo purposes; default nil)
 ;:wait-per-question t
 )
|#