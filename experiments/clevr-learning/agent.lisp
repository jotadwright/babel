;;;; agent.lisp

(in-package :clevr-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass clevr-learning-agent (agent object-w-tasks)
  ((topic :initarg :topic :initform nil
          :accessor topic :type (or null entity)
          :documentation "The answer for the current question")
   (role :initarg :role :initform 'no-role :type symbol :accessor role
         :documentation "The role of the agent (tutor or learner)")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform nil
             :type (or null blackboard)
             :documentation "The agent's ontology"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  () (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((applied-cxns :accessor applied-cxns :initarg :applied-cxns
                 :initform nil :type list
                 :documentation "The cxns used in the current interaction")
   (applied-program :accessor applied-program :initarg :applied-program
                   :initform nil :type list
                   :documentation "The irl-program used in the current interaction")
   (topic-found :accessor topic-found :initarg :topic-found
                :initform nil :type (or null entity)
                :documentation "The answer found by the learner")
   (available-primitives :initarg :available-primitives
                         :accessor available-primitives
                         :initform nil :type (or null primitive-inventory)
                         :documentation "The primitives available for the agent")
   (composer-chunks :initarg :composer-chunks :accessor composer-chunks
                    :initform nil :type list
                    :documentation "The chunks the agent can use for composing"))
  (:documentation "The learner agent"))

(defun default-clevr-grammar ()
  (let ((clevr-grammar (copy-object *CLEVR*)))
    (set-configurations clevr-grammar
                        '((:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                          (:priority-mode . :depth-first))
                        :replace t)
    (set-configurations (processing-cxn-inventory clevr-grammar)
                        '((:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns)
                          (:priority-mode . :depth-first))
                        :replace t)
    clevr-grammar))

(defun make-clevr-learning-tutor (experiment)
  (make-instance 'clevr-learning-tutor :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :ontology (copy-object *clevr-ontology*)))

(defun make-clevr-learning-learner (experiment)
  (let ((learner
         (make-instance 'clevr-learning-learner :role 'learner :experiment experiment
                        :grammar (empty-cxn-set)
                        :ontology (copy-object *clevr-ontology*))))
    (set-primitives-for-current-challenge-level learner)
    (update-composer-chunks-w-primitive-inventory learner)
    learner))

;; -----------------------
;; + Learner Hearer Task +
;; -----------------------

(defclass learner-hearer-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the hearer"))

(defgeneric run-learner-hearer-task (agent)
  (:documentation "Entry point for the tutor speaker task"))

(defmethod run-learner-hearer-task (agent)
  (let* ((task (make-instance 'learner-hearer-task
                              :owner agent
                              :label 'learner-hearer-task
                              :processes '(initial-process
                                           parse
                                           interpret
                                           determine-success
                                           align)
                              :diagnostics (list (make-instance 'parsing-diagnostic))
                              :repairs (list (make-instance 'make-new-program-repair))))
         (all-task-results (object-run-task agent task)))
    ;; there should be only one result
    (find-data (first all-task-results) 'success)))

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

(defclass parsing-problem (problem)
  () (:documentation "Problem created when parsing fails"))

(defclass parsing-diagnostic (diagnostic)
  ((trigger :initform 'parsing))
  (:documentation "Diagnostic to check the result of parsing"))

(defclass make-new-program-repair (repair)
  ((trigger :initform 'parsing))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair uses the composer to create a new program
                   for the current question."))

(define-event parsing-succeeded)
(define-event new-program-repair-started)
(define-event chosen-composer-solution (solution chunk-evaluation-result))

(defmethod diagnose ((diagnostic parsing-diagnostic)
                     process-result &key trigger)
  ;; check if the process result contains an applied-cxn
  (declare (ignorable trigger))
  (if (find-data process-result 'applied-cxns)
    (notify parsing-succeeded)
    (make-instance 'parsing-problem)))

(defmethod repair ((repair make-new-program-repair)
                   (problem parsing-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new cxn and adding it to
  ;; the agent's grammar
  (declare (ignorable trigger))
  (notify new-program-repair-started)
  (let* ((agent (owner (task (process object))))
         (composer-solution (compose-new-program agent (topic agent)))
         (holophrase-chunk (solution->chunk agent composer-solution)))
    (notify chosen-composer-solution composer-solution)
    (push-data (ontology agent) 'chunks holophrase-chunk)
    (add-holophrase-cxn (grammar agent) (utterance agent) holophrase-chunk
                        :initial-score (get-configuration agent :initial-cxn-score))
    (add-composer-chunk agent (chunk (irl::node composer-solution)))))
                                                

(defmethod run-process (process
                        (process-label (eql 'parse))
                        task agent)
  ;; Try to parse the question
  (let (process-result-data)
    (multiple-value-bind (irl-program cipn)
        (comprehend (utterance agent)
                    :cxn-inventory (grammar agent))
      (when (and irl-program (eql (first (statuses cipn)) 'fcg::succeeded))
        (let ((all-applied-cxns (mapcar #'get-original-cxn
                                        (applied-constructions cipn))))
          (push (cons 'applied-cxns all-applied-cxns) process-result-data)
          (push (cons 'irl-program irl-program) process-result-data)
          (setf (applied-cxns agent) all-applied-cxns))))
    (let ((process-result (make-process-result 1 process-result-data :process process)))
      (unless (notify-learning process-result :trigger 'parsing)
        process-result))))

;; --------------------------
;; + Interpretation process +
;; --------------------------

(define-event interpretation-succeeded (answer t))

(defmethod run-process (process
                        (process-label (eql 'interpret))
                        task agent)
  ;; Interpret the parsed irl-program in the current scene
  (let ((irl-program (find-data (input process) 'irl-program))
        process-result-data)
    (when irl-program
       (let ((all-solutions
              (evaluate-irl-program irl-program (ontology agent)
                                    :primitive-inventory (available-primitives agent))))
         (when (length= all-solutions 1)
           (let ((answer (get-target-value irl-program (first all-solutions))))
             (setf (applied-program agent) irl-program
                   (topic-found agent) answer)
             (push (cons 'found-topic answer) process-result-data)
             (notify interpretation-succeeded answer)))))
    (make-process-result 1 process-result-data :process process)))

;; -----------------------------
;; + Determine success process +
;; -----------------------------

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task agent)
  (let ((successp (and (find-data (input process) 'found-topic)
                       (equal-entity (find-data (input process) 'found-topic)
                                     (topic agent)))))
    (make-process-result 1 `((success . ,successp)) :process process)))

;; ---------------------
;; + Alignment process +
;; ---------------------

(defmethod run-process (process
                        (process-label (eql 'align))
                        task agent)
  (let ((successp (find-data (input process) 'success)))
    ;; run the specific alignment strategy
    (run-alignment agent successp (get-configuration agent :alignment-strategy))
    ;; create an empty process result
    (make-process-result 1 nil :process process)))

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))
(define-event adjust-program-started)

(defmethod run-alignment ((agent clevr-learning-learner) success
                          (strategy (eql :lateral-inhibition+store-past-scenes)))
  (notify alignment-started)
  ;; reward or punish the cxns
  (if success
    (progn
      (loop for cxn in (applied-cxns agent)
            do (inc-cxn-score cxn :delta (get-configuration agent :cxn-incf-score)))
      (notify cxns-rewarded (applied-cxns agent))
      (loop for competitor in (get-meaning-competitors agent (applied-cxns agent))
            do (dec-cxn-score agent competitor
                              :delta (get-configuration agent :cxn-decf-score))
            collect competitor into punished-cxns
            finally (notify cxns-punished punished-cxns)))
    (when (applied-cxns agent)
      (loop for cxn in (applied-cxns agent)
            do (dec-cxn-score agent cxn :delta (get-configuration agent :cxn-decf-score)))
      (notify cxns-punished (applied-cxns agent))))
  ;; store the scene
  (let* ((clevr-scene (find-data (ontology agent) 'clevr-context))
         (new-sample (list (index clevr-scene) (utterance agent) (topic agent))))
    (unless (find new-sample (find-data agent 'samples)
                  :test #'(lambda (s1 s2)
                            (and (= (first s1) (first s2))
                                 (string= (second s1) (second s2)))))
      (push-data agent 'samples new-sample)))
  ;; update the program if necessary
  ;; TO DO; when only using holophrase,
  ;; the current cxn can be removed and replaced
  ;; However, when multiple cxns are used
  ;; (after running Jonas' repairs)
  ;; how should the lexicon be updated.
  ;; Only update item-based cxns using the predicates
  ;; (i.e. removing the bind statements?)
  (unless success
    (notify adjust-program-started)
    (let* ((composer-solution (compose-program-update agent (topic agent)))
           (holophrase-chunk (solution->chunk agent composer-solution)))
      (notify chosen-composer-solution composer-solution)
      (push-data (ontology agent) 'chunks holophrase-chunk)
      (add-holophrase-cxn (grammar agent) (utterance agent) holophrase-chunk
                          :initial-score (get-configuration agent :initial-cxn-score))
      (add-composer-chunk agent (chunk (irl::node composer-solution))))))
           
      

