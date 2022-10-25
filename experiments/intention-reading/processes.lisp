;;;; processes.lisp

(in-package :clevr-learning)

;; -------------------
;; + Initial process +
;; -------------------

(defmethod run-process (process
                        (process-label (eql 'initial-process))
                        task agent)
  ;; There needs to be a dummy initial process
  (declare (ignorable process task agent process-label))
  (make-process-result 1 nil :process process))


;; -------------------
;; + Parsing process +
;; -------------------  

(define-event new-cxns-learned (cxns list))
(define-event new-th-links-learned (th type-hierarchy) (links list))
(define-event parsing-finished (process-result process-result))
(define-event interpretation-finished (process-result process-result))

(defmethod run-process (process
                        (process-label (eql 'parse-and-interpret))
                        task (agent clevr-learning-learner))
  ;; store some data in the blackboard for using the interpretation goal test
  (set-data (blackboard (grammar agent)) :ontology (ontology agent))
  (set-data (blackboard (grammar agent)) :primitive-inventory (available-primitives agent))
  (set-data (blackboard (grammar agent)) :ground-truth-topic (topic agent))
  ;; Try to parse the question and (possibly) learn something
  (multiple-value-bind (irl-program cipn)
      (comprehend (utterance agent)
                  :cxn-inventory (grammar agent))
    ;; clear the blackboard again (to be sure)
    (set-data (blackboard (grammar agent)) :ontology nil)
    (set-data (blackboard (grammar agent)) :primitive-inventory nil)
    (set-data (blackboard (grammar agent)) :ground-truth-topic nil)
    ;; Gather the process result data. There can be multiple
    ;; goal-test-failed nodes. Take the one that was created last.
    (let* ((cip-node
            (if (find 'fcg::succeeded (statuses cipn)) cipn
              (the-biggest #'created-at
                           (find-all 'fcg::goal-test-failed
                                     (all-cip-nodes (cip cipn))
                                     :key #'statuses :test #'member))))
           (process-result-data
            `((cipn . ,cip-node)
              (applied-cxns . ,(when (applied-constructions cip-node)
                                 (original-applied-constructions cip-node)))
              (irl-program . ,irl-program)
              ;; the interpretation goal test stored the computed topic
              ;; in goal-test-data, so we don't need to compute it again
              (computed-topic . ,(find-data (goal-test-data cip-node) :computed-topic))))
           (process-result
            (make-process-result 1 process-result-data :process process))
           (all-statuses (when cip-node (all-cipn-statuses cip-node)))
           (repair-applied-p (find 'fcg::added-by-repair all-statuses)))
      ;; notify the repair
      (when repair-applied-p
        (let ((fix-cxns (find-data (car-resulting-cfs (cipn-car cip-node)) :fix-cxns))
              (fix-th-links (find-data (car-resulting-cfs (cipn-car cip-node)) :fix-th-links)))
          (when fix-cxns
            (notify new-cxns-learned fix-cxns))
          (when fix-th-links
            (notify new-th-links-learned (get-type-hierarchy (construction-inventory cip-node)) fix-th-links))))
      ;; notify that parsing has finished
      (notify parsing-finished process-result)
      ;; notify that interpretation has finished
      (notify interpretation-finished process-result)
      ;; return the process result
      process-result)))


;; -------------------------
;; + Parse process (tutor) +
;; -------------------------

(defmethod run-process (process
                        (process-label (eql 'parse))
                        task (agent clevr-learning-tutor))
  (multiple-value-bind (irl-program cipn)
      (comprehend (utterance agent)
                  :cxn-inventory (grammar agent)
                  :silent t)
    (make-process-result 1 `((cipn . ,cipn)
                             (irl-program . ,irl-program)
                             (applied-cxns . ,(original-applied-constructions cipn)))
                         :process process)))

;; --------------------------
;; + Interpretation process +
;; --------------------------

(defmethod run-process (process
                        (process-label (eql 'interpret))
                        task agent)
  ;; Interpret the parsed irl-program in the current scene
  (let ((computed-topic nil)
        (process-result nil))
    (multiple-value-bind (irl-program foundp)
        (find-data (input process) 'irl-program)
      (when foundp
        (let* ((all-solutions
                (evaluate-irl-program irl-program (ontology agent)
                                      :primitive-inventory (available-primitives agent)
                                      :silent (tutorp agent))))
          ;; there should only be a single solution
          (when (length= all-solutions 1)
            (let ((answer (get-target-value irl-program (first all-solutions))))
              (setf computed-topic answer)))))
      (setf process-result
            (make-process-result 1 `((computed-topic . ,computed-topic))
                                 :process process))
      (notify interpretation-finished process-result)
      process-result)))



;; ----------------------
;; + Production process +
;; ----------------------

(defun composer-solution-can-be-formulated-p (solution agent)
  (let ((irl-program
         (append (bind-statements solution)
                 (irl-program (chunk solution)))))
    (multiple-value-bind (utterance cipn)
        (formulate (fcg::instantiate-variables irl-program)
                   :cxn-inventory (grammar agent) :silent t)
      (declare (ignorable utterance))
      (find 'fcg::succeeded (fcg::statuses cipn)))))

(define-event production-finished (process-result process-result))

(defmethod run-process (process
                        (process-label (eql 'produce))
                        task agent)
  "Compose a program that leads to the topic
   until formulation of that program is successful."
  ;; only try to produce when there are cxns in the grammar
  (when (constructions (grammar agent))
    (let (process-result)
      ;; compose programs until it can be formulated
      (let* ((composer
              (make-default-composer agent (topic agent)))
             (composer-solution
              (compose-until
               composer
               (lambda (solution index)
                 (declare (ignorable index))
                 (composer-solution-can-be-formulated-p solution agent)))))
        ;; when a program can be formulated, extract all relevant information
        ;; and continue with routine processing
        (when composer-solution
          (let ((irl-program
                 (fcg::instantiate-variables
                  (append (bind-statements composer-solution)
                          (irl-program (chunk composer-solution))))))
            (multiple-value-bind (utterance cipn)
                (formulate irl-program :cxn-inventory (grammar agent))
              (let* ((processed-utterance
                      (handle-clevr-punctuation
                       (list-of-strings->string utterance)))
                     (applied-cxns (original-applied-constructions cipn))
                     (process-result-data
                      `((utterance . ,processed-utterance)
                        (irl-program . ,irl-program)
                        (cipn . ,cipn)
                        (applied-cxns . ,applied-cxns))))
                ;; make the process result
                (setf process-result
                      (make-process-result 1 process-result-data
                                           :process process))
                (notify production-finished process-result))))))
      process-result)))



;; -----------------------------
;; + Determine success process +
;; -----------------------------

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task agent)
  ;; Check if the computed answer matches the ground truth answer
  ;; and no repairs have been applied. The add-th-links repair
  ;; does not 'count' as a repair. It will not add the 'added-by-repair'
  ;; status to any node.
  (let* ((cipn (find-data (input process) 'cipn))
         (cipn-succeeded (find 'fcg::succeeded (statuses cipn)))
         (all-statuses (when cipn (all-cipn-statuses cipn)))
         (repair-applied-p (find 'fcg::added-by-repair all-statuses))
         (failed-interpretation-p (find 'failed-interpretation-problem all-statuses))
         (computed-topic (find-data (input process) 'computed-topic))
         (correct-topic (equal-entity computed-topic (topic agent)))
         (success (and cipn-succeeded computed-topic correct-topic (not repair-applied-p))))
    (make-process-result 1 `((success . ,success)
                             (repair-applied . ,repair-applied-p)
                             (failed-interpretation . ,failed-interpretation-p)
                             (correct-topic . ,correct-topic))
                         :process process)))

;; ---------------------
;; + Alignment process +
;; ---------------------

(defmethod run-process (process
                        (process-label (eql 'hearer-align))
                        task agent)
  ;; run the alignment strategy
  (run-hearer-alignment agent (input process)
                        (get-configuration agent :alignment-strategy))
  (make-process-result 1 nil :process process))


;; -------------------------------
;; + Receive Gold Answer process +
;; -------------------------------

(defmethod run-process (process
                        (process-label (eql 'receive-gold-answer))
                        task agent)
  (let ((computed-topic (find-data task 'gold-answer))
        (applied-cxns (find-data task 'applied-cxns))
        (cipn (find-data task 'cipn))
        (irl-program (find-data task 'irl-program)))
    (make-process-result 1 `((computed-topic . ,computed-topic)
                             (applied-cxns . ,applied-cxns)
                             (cipn . ,cipn)
                             (irl-program . ,irl-program))
                         :process process)))


;; -------------------------
;; + Speaker Align process +
;; -------------------------

(defmethod run-process (process
                        (process-label (eql 'speaker-align))
                        task agent)
  ;; run the alignment strategy
  (run-speaker-alignment agent (input process)
                         (get-configuration agent :alignment-strategy))
  (make-process-result 1 nil :process process))

