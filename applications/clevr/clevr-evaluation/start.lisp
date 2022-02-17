(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; FCG MONITORS
(activate-monitor trace-fcg)
;; IRL MONITORS
(activate-monitor trace-irl)
;; CLEVR EVALUATION MONITOR
(activate-monitor trace-clevr-evaluation)

(defgeneric evaluate-clevr-coverage (data-split direction
                                     &key nr-of-questions)
  (:documentation "Evaluate the coverage of the clevr grammar."))

(defmethod evaluate-clevr-coverage (data-split (direction (eql '<-))
                                    &key nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t)))
    (average
     (remove nil
             (loop with processed-questions = 0
                   for filename in (question-sets clevr-world)
                   for set-of-questions = (load-clevr-question-set filename)
                   if (and nr-of-questions (>= processed-questions nr-of-questions))
                   return coverage
                   else
                   append (loop for clevr-question in (questions set-of-questions)
                                for q = (question clevr-question)
                                for (nil cipn nil)
                                = (multiple-value-list
                                   (understand q *clevr* '?scene))
                                do (incf processed-questions)
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                return question-set-coverage
                                else if (find 'fcg::succeeded (fcg::statuses cipn))
                                collect 1 into question-set-coverage
                                else collect 0 into question-set-coverage
                                finally return question-set-coverage)
                   into coverage
                   finally return coverage)))))
    

(defmethod evaluate-clevr-coverage (data-split (direction (eql '->))
                                    &key nr-of-questions)
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t)))
    (average
     (remove nil
             (loop with processed-questions = 0
                   for filename in (question-sets clevr-world)
                   for set-of-questions = (load-clevr-question-set filename)
                   if (and nr-of-questions (>= processed-questions nr-of-questions))
                   return coverage
                   else
                   append (loop for clevr-question in (questions set-of-questions)
                                for q = (question clevr-question)
                                for (nil cipn nil)
                                = (multiple-value-list
                                   (understand-and-formulate q *clevr* '?scene))
                                do (incf processed-questions)
                                if (and nr-of-questions (>= processed-questions nr-of-questions))
                                return question-set-coverage
                                else if (find 'fcg::succeeded (fcg::statuses cipn))
                                collect 1 into question-set-coverage
                                else collect 0 into question-set-coverage
                                finally return question-set-coverage)
                   into coverage
                   finally return coverage)))))



(defun compute-answer (irl-program scene-var scene-path)
  (let ((solutions
         (evaluate-irl-program
          (cons `(bind pathname-entity ,scene-var ,scene-path) irl-program)
          *clevr-ontology* :primitive-inventory *clevr-primitives*)))
    (when (and solutions (length= solutions 1))
      (let* ((target-var (get-target-var irl-program))
             (target-value (value (find target-var (first solutions) :key #'var))))
        (answer->str target-value)))))

(defun extract-scene-unit-variable (cipn)
  "returns scene-variable from resulting cfs of given cipn"
  (let* ((cfs (pole-structure (left-pole (car-resulting-cfs (cipn-car cipn)))))
         (scene-unit (find 'fcg::scene-unit cfs :key #'first))
         (scene-var (second (find 'fcg::scene (rest scene-unit) :key #'first))))
    scene-var))

(defgeneric evaluate-clevr-accuracy (data-split &key nr-of-scenes nr-of-questions)
  (:documentation "Evaluate the accuracy of the clevr grammar."))

(defmethod evaluate-clevr-accuracy (data-split &key nr-of-scenes nr-of-questions)
  ;; accuracy in comprehension
  ;; loop over scenes and questions
  ;;      do comprehension
  ;;      evaluate irl program
  ;;      compare meaning to ground-truth
  ;;      compare answer to ground-truth
  (let ((clevr-world
         (make-instance 'clevr-world
                        :data-sets (list data-split)
                        :load-questions t)))
    (average
     (remove nil
             (loop for scene-path in (scenes clevr-world)
                   for question-path in (question-sets clevr-world)
                   for set-of-questions = (load-clevr-question-set question-path)
                   for path-entity = (make-instance 'pathname-entity :pathname scene-path)
                   append (loop for clevr-question in (questions set-of-questions)
                                for q = (question clevr-question)
                                for answer = (answer clevr-question)
                                for (irl-program cipn nil)
                                = (multiple-value-list
                                   (clevr-grammar::understand q *clevr* '?scene))
                                for scene-var = (extract-scene-unit-variable cipn)
                                when (find 'fcg::succeeded (fcg::statuses cipn))
                                if (string= (upcase answer)
                                            (upcase (compute-answer irl-program scene-var path-entity)))
                                collect 1 else collect 0))))))
                   
          
  