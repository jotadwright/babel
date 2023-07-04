;;;; interaction.lisp

(in-package :pattern-finding-old)

;; ----------------------
;; + Before Interaction +
;; ----------------------

(defmethod initialize-agent ((agent pattern-finding-agent)
                             utterance gold-standard-meaning)
  (setf (utterance agent) utterance
        (meaning agent) gold-standard-meaning
        (communicated-successfully agent) t))

(defun get-interaction-data (interaction)
  "retrieve the nth utterance and gold standard meaning from the dataset"
  (let* ((sample (nth (- (interaction-number interaction) 1)
                      (corpus (experiment interaction))))
         (utterance (car sample))
         (gold-standard-meaning (cdr sample)))
    (values utterance gold-standard-meaning)))

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defmethod interact :before ((experiment pattern-finding-experiment)
                             interaction &key)
  (multiple-value-bind (utterance gold-standard-meaning)
      (get-interaction-data interaction)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (set-data interaction :utterance utterance)
    (set-data interaction :gold-standard-meaning gold-standard-meaning)
    (notify interaction-before-finished utterance gold-standard-meaning)))

;; ---------------
;; + Interaction +
;; ---------------

(defun determine-communicative-success (cipn)
  (assert (find 'fcg::succeeded (statuses cipn)))
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn))))
        (communicative-success t))
    (when (find 'gold-standard-consulted all-node-statuses)
      (setf communicative-success nil))
    communicative-success))

(defun get-last-repair-symbol (cipn success?)
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn)))))
    (if (not (find 'added-by-repair all-node-statuses))
      (if success? "." "x") ; return a dot or x in processing mode
      (cond ((member 'nothing->holistic all-node-statuses) "h")
            ((member 'anti-unify-cxns all-node-statuses) "a")
            ((member 'anti-unify-cipn all-node-statuses) "p")
            ((member 'add-categorial-links all-node-statuses) "l")))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(define-event constructions-chosen (constructions list))
(define-event cipn-statuses (cipn-statuses list))

(defmethod interact ((experiment pattern-finding-experiment)
                     interaction &key)
  "the learner attempts to comprehend the utterance with its grammar,
   and applies repairs if necessary"
  (let ((agent (first (population experiment))))
    ;; set the current interaction number in the blackboard of the grammar
    (set-data (blackboard (grammar agent))
              :current-interaction-nr (interaction-number interaction))
    ;; run comprehension
    (multiple-value-bind (meanings cipns)
        (comprehend-all (utterance agent)
                        :cxn-inventory (grammar agent)
                        :gold-standard-meaning (meaning agent))
      (declare (ignore meanings))
      (let* ((solution-cipn (first cipns))
             (competing-cipns (rest cipns))
             (applied-cxns (original-applied-constructions solution-cipn))
             (successp (determine-communicative-success solution-cipn)))
        ;; notify
        (notify constructions-chosen applied-cxns)
        (notify cipn-statuses (statuses solution-cipn))
        ;; run alignment
        (run-alignment agent solution-cipn competing-cipns
                       (get-configuration agent :alignment-strategy))
        ;; update the :last-used property of the cxns
        (dolist (cxn applied-cxns)
          (set-cxn-last-used agent cxn))
        ;; store applied repair
        (set-data interaction :applied-repair (get-last-repair-symbol solution-cipn successp))
        ;; set the success of the agent
        (setf (communicated-successfully agent) successp)))))