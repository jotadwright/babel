(in-package :pattern-finding-old)

(defclass pattern-finding-agent (agent)
  ((meaning :initarg :meaning :initform nil
            :accessor meaning :type (or null entity number)
            :documentation "The meaning representation for the current utterance")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")))

(defun make-pattern-finding-agent (experiment)
  (make-instance 'pattern-finding-agent
                 :experiment experiment
                 :grammar (empty-cxn-set experiment)))

(define-event constructions-chosen (constructions list))
(define-event cipn-statuses (cipn-statuses list))
(define-event log-parsing-finished
  (agent pattern-finding-agent)
  (process-result-data list))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(defgeneric run-learner-comprehension-task (agent)
  (:documentation "Entry point for the learner's comprehension task"))

(defmethod run-learner-comprehension-task (agent)
  ;; set the current interaction number of the blackboard of the grammar
  (set-data (blackboard (grammar agent))
            :current-interaction-nr (interaction-number (current-interaction (experiment agent))))
  ;; run comprehension
  (multiple-value-bind (meanings cipns)
      (comprehend-all (utterance agent)
                      :cxn-inventory (grammar agent)
                      :gold-standard-meaning (meaning agent)
                      :n (get-configuration (experiment agent) :comprehend-all-n))
    (declare (ignore meanings))
    (let* ((solution-cipn (first cipns))
           (competing-cipns (rest cipns))
           (applied-cxns (original-applied-constructions solution-cipn)))
      ;; notify the logging monitor
      ;; notify which cxns will be used
      (notify constructions-chosen applied-cxns)
      (notify cipn-statuses (statuses solution-cipn))

      ;; do alignment
      (run-alignment agent solution-cipn competing-cipns
                     (get-configuration (experiment agent) :alignment-strategy))
      ;(notify-learning process-result :trigger 'alignment-finished)
      
      ;; update the :last-used property of the cxns
      (loop for cxn in applied-cxns
            do (set-cxn-last-used agent cxn))
      
    solution-cipn)))
     


