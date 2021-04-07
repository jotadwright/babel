;;;; agent.lisp

(in-package :clevr-learning)

;; -----------------
;; + Agent Classes +
;; -----------------

(defclass clevr-learning-agent (agent object-w-tasks)
  ((topic :initarg :topic :initform nil
          :accessor topic :type (or null entity number)
          :documentation "The answer for the current question")
   (role :initarg :role :initform 'no-role :type symbol :accessor role
         :documentation "The role of the agent (tutor or learner)")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")
   (ontology :initarg :ontology :accessor ontology :initform nil
             :type (or null blackboard)
             :documentation "The agent's ontology")
   (question-success-table
    :initarg :success-table :initform nil :type list
    :accessor question-success-table
    :documentation "Agent keeps track of which questions
                               were seen, and how often it was
                               successful or not")
   (current-question-index :initform -1 :accessor current-question-index
                           :type number :documentation "Index of current question")
   (available-primitives :initarg :available-primitives
                         :accessor available-primitives
                         :initform nil :type (or null primitive-inventory)
                         :documentation "The primitives available for the agent"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  () (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((task-result :initarg :task-result
                :accessor task-result
                :initform nil
                :documentation "Pointer to the result of the task in this interaction")
   (memory :initarg :memory :accessor memory :initform (make-hash-table :test #'eq)
           :documentation "The agent's memory (used by composer strategy)")
   (composer-chunks :initarg :composer-chunks :accessor composer-chunks
                    :initform nil :type list
                    :documentation "The chunks the agent can use for composing"))
  (:documentation "The learner agent"))

(defun default-clevr-grammar ()
  (let ((clevr-grammar (copy-object *CLEVR*)))
    (set-configurations clevr-grammar
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    (set-configurations (processing-cxn-inventory clevr-grammar)
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    clevr-grammar))

(defun make-clevr-learning-tutor (experiment)
  (make-instance 'clevr-learning-tutor
                 :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :ontology (copy-object *clevr-ontology*)
                 :success-table (loop for i below (length (question-data experiment))
                                      collect (cons i nil))
                 :available-primitives (copy-object *clevr-primitives*)))

(defun make-clevr-learning-learner (experiment)
  (let ((learner
         (make-instance 'clevr-learning-learner
                        :role 'learner :experiment experiment
                        :grammar (empty-cxn-set (get-configuration experiment :hide-type-hierarchy)
                                                (get-configuration experiment :learner-cxn-supplier))
                        :ontology (copy-object *clevr-ontology*)
                        :success-table (loop for i below (length (question-data experiment))
                                             collect (cons i nil)))))
    (set-primitives-for-current-challenge-level learner)
    (update-composer-chunks-w-primitive-inventory learner)
    learner))

(defmethod clear-question-success-table ((agent clevr-learning-agent))
  (setf (question-success-table agent)
        (loop for i below (length (question-data (experiment agent)))
              collect (cons i nil))))

(defmethod clear-memory ((agent clevr-learning-learner))
  (setf (memory agent)
        (make-hash-table :test #'eq)))

;; -----------------------
;; + Learner Hearer Task +
;; -----------------------

(defclass learner-hearer-task (task-w-learning)
  () (:documentation "Task executed by the learner when it is the hearer"))

(defgeneric run-learner-hearer-task (agent)
  (:documentation "Entry point for the tutor speaker task"))

(defmethod run-learner-hearer-task (agent)
  (let* ((diagnostics
          (loop for diagnostic in '(diagnose-parsing-result
                                    diagnose-aligment-result)
                collect (make-instance diagnostic)))
         (repairs
          (loop for repair in '(repair-add-th-links
                                repair-item-based->lexical
                                repair-lexical->item-based
                                repair-holophrase->item-based-substitution
                                repair-holophrase->item-based-addition
                                repair-holophrase->item-based-deletion
                                repair-item-based+lexical->item-based
                                repair-make-hypotheses
                                repair-make-holophrase-cxn)
                collect (make-instance repair)))
         (task
          (make-instance 'learner-hearer-task
                         :owner agent
                         :label 'parse-interpret-and-align-task
                         :processes '(initial-process
                                      parse
                                      interpret
                                      determine-success
                                      align)
                         :diagnostics diagnostics
                         :repairs repairs))
         (all-task-results (object-run-task agent task)))
    ;; there should be only one result
    (setf (task-result agent) (first all-task-results))
    (find-data (task-result agent) 'success)))

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

(defclass diagnose-parsing-result (diagnostic)
  ((trigger :initform 'parsing-finished))
  (:documentation "Diagnostic to check the result of parsing"))

(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails and there are no applied cxns"))

(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails and there are some applied cxns"))

(define-event parsing-succeeded)      

(defmethod diagnose ((diagnostic diagnose-parsing-result)
                     process-result &key trigger)
  ;; check if parsing succeeded
  ;; a different repair is triggered depending on
  ;; whether no cxns could apply
  ;; or some cxns could apply
  (declare (ignorable trigger))
  (cond ((null (get-data process-result 'applied-cxns))
         (make-instance 'unknown-utterance-problem))
        ((find 'fcg::succeeded (fcg:statuses (get-data process-result 'cipn)))
         (notify parsing-succeeded))
        ((not (null (get-data process-result 'applied-cxns)))
         (make-instance 'partial-utterance-problem))))


(defclass repair-make-holophrase-cxn (repair)
  ((trigger :initform '(parsing-finished alignment-finished)))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair uses the composer to create a new program
                   for the current question."))

(defmethod trigger? ((repair repair-make-holophrase-cxn) trigger &key &allow-other-keys)
  (member trigger (trigger repair)))

(define-event add-holophrase-repair-started)
(define-event add-holophrase-new-cxn
  (cxn construction))
    
(defmethod repair ((repair repair-make-holophrase-cxn)
                   (problem unknown-utterance-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new holophrase cxn and adding it to
  ;; the agent's grammar
  (declare (ignorable trigger))
  (notify add-holophrase-repair-started)
  (let ((agent (owner (task (process object)))))
    (multiple-value-bind (cxns th-links)
        (run-repair agent (get-data object 'applied-cxns)
                    (get-data object 'cipn) :add-holophrase)
      (declare (ignorable th-links))
      (notify lexicon-changed)
      (loop for cxn in cxns
            do (add-cxn cxn (grammar agent)))
      (notify add-holophrase-new-cxn (first cxns))
      (make-instance 'fix :issued-by repair :problem problem))))


(defclass repair-lexical->item-based (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to create an item-based cxn,
                   given a lexical cxn"))
                         
(define-event lexical->item-based-repair-started)
(define-event lexical->item-based-new-cxn-and-links
  (cxn construction) (th type-hierarchy)
  (new-links list))

(defmethod repair ((repair repair-lexical->item-based)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn)))
    ;; only lexical cxns applied -> make an item-based cxn
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :lexical->item-based)
      (when (and cxns th-links)
        (notify lexical->item-based-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify lexical->item-based-new-cxn-and-links
                (first cxns) (get-type-hierarchy (grammar agent))
                th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))



(defclass repair-item-based->lexical (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to create a lexical cxn, given
                   an item-based cxn"))

(define-event item-based->lexical-repair-started)
(define-event item-based->lexical-new-cxn-and-th-links
  (cxn construction) (th type-hierarchy)
  (new-links list))

(defmethod repair ((repair repair-item-based->lexical)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn)))
    ;;  item-based cxn applied and root not empty -> make a lexical cxn
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :item-based->lexical)
      (when cxns
        (notify item-based->lexical-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify item-based->lexical-new-cxn-and-th-links
                (first cxns) (get-type-hierarchy (grammar agent))
                th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))



(defclass repair-add-th-links (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to add th links"))

(define-event add-th-links-repair-started)
(define-event add-th-links-new-th-links
  (th type-hierarchy) (new-links list))

(defmethod repair ((repair repair-add-th-links)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; item-based and lexical-cxn without solution
  ;; => add th links!
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn)))
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :add-th-links)
      (declare (ignorable cxns))
      (when th-links
        (notify add-th-links-repair-started)
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify add-th-links-new-th-links (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem :restart-data t)))))


(defmethod repair ((repair repair-make-holophrase-cxn)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new holophrase cxn and adding it to
  ;; the agent's grammar
  (declare (ignorable trigger))
  (notify add-holophrase-repair-started)
  (let ((agent (owner (task (process object)))))
    (multiple-value-bind (cxns th-links)
        (run-repair agent (get-data object 'applied-cxns)
                    (get-data object 'cipn) :add-holophrase)
      (declare (ignorable th-links))
      (notify lexicon-changed)
      (loop for cxn in cxns
            do (add-cxn cxn (grammar agent)))
      (notify add-holophrase-new-cxn (first cxns))
      (make-instance 'fix :issued-by repair :problem problem))))



(defun get-all-non-duplicate-leaf-nodes (cip)
  (remove-if #'(lambda (node) (find 'fcg::duplicate (fcg::statuses node)))
             (remove nil (traverse-depth-first
                          cip  :collect-fn #'(lambda (node)
                                               (when (null (children node))
                                                 node))))))

(defun separate-nodes (leaf-nodes)
  ;; remove node if item-based can never cover the current utterance
  ;; look at the number of slots, the number of applied lex cxns
  ;; and the number of strings in root
  ;; when nr-of-slots < applied lex + strings in root, skip this node
  ;; when nr-of-slots > applied lex + 1, skip this node
  (loop for node in leaf-nodes
        for applied-cxns = (mapcar #'get-original-cxn
                                   (applied-constructions node))
        for applied-lex-cxns = (find-all 'lexical applied-cxns :key #'get-cxn-type)
        for applied-item-based-cxn = (find 'item-based applied-cxns :key #'get-cxn-type)
        for strings-in-root = (get-strings-from-root node)
        when (and applied-item-based-cxn
                  (or (< (item-based-number-of-slots applied-item-based-cxn)
                         (+ (length applied-lex-cxns)
                            (length strings-in-root)))
                      (> (item-based-number-of-slots applied-item-based-cxn)
                         (1+ (length applied-lex-cxns)))))
        collect node into impossible-nodes
        else collect node into possible-nodes
        finally (return (values possible-nodes impossible-nodes))))

(defun all-applied-cxns (cipn)
  (cond (;initial node
         (and (null (parent cipn))
              (null (children cipn)))
         (values nil nil))
        (;success node
         (find 'fcg::succeeded (fcg::statuses cipn))
         (values (mapcar #'get-original-cxn
                         (applied-constructions cipn))
                 cipn))
        (t ;otherwise, take all non-duplicate leaf nodes
         (let ((all-leaf-nodes
                (get-all-non-duplicate-leaf-nodes (cip cipn))))
           (if (length= all-leaf-nodes 1)
             ;; if there is only one, return that one
             (values (mapcar #'get-original-cxn
                             (applied-constructions (first all-leaf-nodes)))
                     (first all-leaf-nodes))
             ;; else, ...
             (flet ((avg-cxn-scores (node)
                      (average (mapcar #'cxn-score
                                       (mapcar #'get-original-cxn
                                               (applied-constructions node)))))
                    (sum-cxn-scores (node)
                      (reduce #'+ (mapcar #'cxn-score
                                       (mapcar #'get-original-cxn
                                               (applied-constructions node))))))
               (multiple-value-bind (possible-nodes impossible-nodes)
                   (separate-nodes all-leaf-nodes)
                 (let* ((set-to-consider
                         (if possible-nodes possible-nodes impossible-nodes))
                        (high-score-node
                         (the-biggest #'sum-cxn-scores set-to-consider)))
                   (values (mapcar #'get-original-cxn
                                   (applied-constructions high-score-node))
                           high-score-node)))))))))
         
(define-event constructions-chosen (constructions list))
(define-event log-parsing-finished
  (agent clevr-learning-agent)
  (process-result-data list))

(defmethod run-process (process
                        (process-label (eql 'parse))
                        task
                        (agent clevr-learning-learner))
  ;; Try to parse the question
  (multiple-value-bind (irl-program cipn)
      (comprehend (utterance agent) :cxn-inventory (grammar agent))
    ;; lexical cxns could have applied, providing a partial program
    ;; that further restricts the composer, so we always put this
    ;; in the process result
    (let* ((process-result-data
            (multiple-value-bind (applied-cxns cipn) (all-applied-cxns cipn)
              `((cipn . ,cipn)
                (applied-cxns . ,applied-cxns)
                (irl-program . ,irl-program))))
           (process-result
            (make-process-result 1 process-result-data :process process)))
      ;; notify the logging monitor
      (notify log-parsing-finished agent process-result-data)
      ;; notify which cxns will be used
      (notify constructions-chosen (rest (assoc 'applied-cxns process-result-data)))
      ;; update the :last-used property of the cxns
      (loop for cxn in (rest (assoc 'applied-cxns process-result-data))
            do (set-cxn-last-used agent cxn))
      ;; check if a repair needs to be triggered
      (unless (notify-learning process-result :trigger 'parsing-finished)
        process-result))))

;; --------------------------
;; + Interpretation process +
;; --------------------------

(define-event interpretation-succeeded (answer t))

(defmethod run-process (process
                        (process-label (eql 'interpret))
                        task agent)
  ;; Interpret the parsed irl-program in the current scene
  (let ((found-topic nil))
    (when (find-data (input process) 'irl-program)
       (let* ((irl-program (find-data (input process) 'irl-program))
              (all-solutions
               (evaluate-irl-program irl-program (ontology agent)
                                     :primitive-inventory (available-primitives agent))))
         ;; there should only be a single solution
         (when (length= all-solutions 1)
           (let ((answer (get-target-value irl-program (first all-solutions))))
             (setf found-topic answer)
             (notify interpretation-succeeded answer)))))
    (make-process-result 1 `((found-topic . ,found-topic)) :process process)))

;; -----------------------------
;; + Determine success process +
;; -----------------------------

(define-event log-interaction-finished
  (agent clevr-learning-agent)
  (process-input blackboard)
  (success t))

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task agent)
  ;; Check if the computer answer matches the ground truth answer
  (let ((success (and (find-data (input process) 'found-topic)
                      (equal-entity (find-data (input process) 'found-topic)
                                    (topic agent)))))
    (notify log-interaction-finished agent (input process) success)
    (make-process-result 1 `((success . ,success)) :process process)))

;; ---------------------
;; + Alignment process +
;; ---------------------

(defmethod run-process (process
                        (process-label (eql 'align))
                        task agent)
  ;; run the alignment strategy
  (run-alignment agent (input process) (get-configuration agent :alignment-strategy))
  (let ((process-result
         (make-process-result 1 nil :process process)))
    (notify-learning process-result :trigger 'alignment-finished)
    process-result))

(defclass diagnose-aligment-result (diagnostic)
  ((trigger :initform 'alignment-finished))
  (:documentation "Diagnostic to check the result of alignment"))

(defclass possible-generalisation-problem (problem)
  () (:documentation "Problem created when alignment was a success
                      and it is possible to generalise over the grammar."))

(defclass failed-interaction-problem (problem)
  () (:documentation "Problem created when the interaction was completed,
but not successful"))

(defmethod diagnose ((diagnostic diagnose-aligment-result)
                     process-result &key trigger)
  ;; check if the interaction succeeded
  ;; a different repair is triggered depending on success
  ;; when no success, a different repair is triggered
  ;; depending on the applied cxns
  (declare (ignorable trigger))
  (if (find-data process-result 'success)
    (make-instance 'possible-generalisation-problem)
    (make-instance 'failed-interaction-problem)))




(defclass repair-make-hypotheses (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when the interaction was not successful"))

(define-event make-hypotheses-repair-started)
(define-event make-hypotheses-new-cxns-and-th-links
  (new-cxns list) (th type-hierarchy) (new-links list))

(defmethod repair ((repair repair-make-hypotheses)
                   (problem failed-interaction-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let ((agent (owner (task (process object)))))
    (multiple-value-bind (cxns th-links)
        (run-repair agent (get-data object 'applied-cxns)
                    (get-data object 'cipn) :add-hypotheses)
      (when (and cxns th-links)
        (notify make-hypotheses-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify make-hypotheses-new-cxns-and-th-links cxns
                (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))



(defmethod repair ((repair repair-make-holophrase-cxn)
                   (problem failed-interaction-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new holophrase cxn and adding it to
  ;; the agent's grammar
  (declare (ignorable trigger))
  (notify add-holophrase-repair-started)
  (let ((agent (owner (task (process object)))))
    (multiple-value-bind (cxns th-links)
        (run-repair agent (get-data object 'applied-cxns)
                    (get-data object 'cipn) :add-holophrase)
      (declare (ignorable th-links))
      (notify lexicon-changed)
      (loop for cxn in cxns
            do (add-cxn cxn (grammar agent)))
      (notify add-holophrase-new-cxn (first cxns))
      (make-instance 'fix :issued-by repair :problem problem))))

(defclass repair-item-based+lexical->item-based (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using substitution."))

(define-event item-based+lexical->item-based-repair-started)
(define-event item-based+lexical->item-based-new-cxns-and-th-links
  (new-cxns list) (th type-hierarchy) (new-links list))


(defmethod repair ((repair repair-item-based+lexical->item-based)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; item-based + lexical -> item-based
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn)))
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :item-based+lexical->item-based)
      (when (and cxns th-links)
        (notify item-based+lexical->item-based-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify item-based+lexical->item-based-new-cxns-and-th-links
                cxns (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))



(defclass repair-holophrase->item-based-substitution (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using substitution."))

(define-event holophrase->item-based-substitution-repair-started)
(define-event holophrase->item-based-subsititution-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy) (new-links list))

(defmethod repair ((repair repair-holophrase->item-based-substitution)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with substitution
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn)))
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :holophrase->item-based--substitution)
      (when (and cxns th-links)
        (notify holophrase->item-based-substitution-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify holophrase->item-based-subsititution-new-cxn-and-th-links
                cxns (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))


(defclass repair-holophrase->item-based-addition (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using addition."))

(define-event holophrase->item-based-addition-repair-started)
(define-event holophrase->item-based-addition-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy) (new-links list))

(defmethod repair ((repair repair-holophrase->item-based-addition)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with addition
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn)))
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :holophrase->item-based--addition)
      (when (and cxns th-links)
        (notify holophrase->item-based-addition-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify holophrase->item-based-addition-new-cxn-and-th-links
                cxns (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))



(defclass repair-holophrase->item-based-deletion (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using deletion."))

(define-event holophrase->item-based-deletion-repair-started)
(define-event holophrase->item-based-deletion-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy) (new-links list))

(defmethod repair ((repair repair-holophrase->item-based-deletion)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with addition
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn)))
    (multiple-value-bind (cxns th-links)
        (run-repair agent applied-cxns cipn :holophrase->item-based--deletion)
      (when (and cxns th-links)
        (notify holophrase->item-based-deletion-repair-started)
        (loop for cxn in cxns
              do (add-cxn cxn (grammar agent)))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              with initial-weight = (get-configuration agent :initial-th-link-weight)
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight initial-weight))
        (notify holophrase->item-based-deletion-new-cxn-and-th-links
                cxns (get-type-hierarchy (grammar agent)) th-links)
        (make-instance 'fix :issued-by repair :problem problem)))))

  

