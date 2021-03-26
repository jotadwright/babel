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
             :documentation "The agent's ontology"))
  (:documentation "Base class for both agents"))

(defclass clevr-learning-tutor (clevr-learning-agent)
  ((question-index-table :initarg :table :initform nil
                         :accessor question-index-table
                         :type list
                         :documentation "Tutor keeps track of which questions the
                                         learner has seen, and how often it was
                                         successful or not")
   (current-question-index :initform -1 :accessor current-question-index
                           :type number :documentation "Index of current question"))
  (:documentation "The tutor agent"))

(defclass clevr-learning-learner (clevr-learning-agent)
  ((task-result :initarg :task-result
                :accessor task-result
                :initform nil
                :documentation "Pointer to the result of the task in this interaction")
   (memory :initarg :memory :accessor memory :initform (make-hash-table :test #'eq)
           :documentation "The agent's memory (used by composer strategy)")
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
  (make-instance 'clevr-learning-tutor
                 :role 'tutor :experiment experiment
                 :grammar (default-clevr-grammar)
                 :ontology (copy-object *clevr-ontology*)
                 :table (loop for i below (length (question-data experiment))
                              collect (cons i nil))))

(defun make-clevr-learning-learner (experiment)
  (let* ((hide-type-hierarchy
          (get-configuration experiment :hide-type-hierarchy))
         (learner
          (make-instance 'clevr-learning-learner
                         :role 'learner :experiment experiment
                         :grammar (empty-cxn-set hide-type-hierarchy)
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
                                repair-make-hypotheses
                                repair-make-holophrase-cxn)
                collect (make-instance repair)))
         (task
          (make-instance 'learner-hearer-task
                         :owner agent
                         :label 'learner-hearer-task
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
      (make-instance 'fix
                     :issued-by repair
                     :problem problem
                     ;:restart-data holophrase-cxn
                     ))))


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
        (notify lexicon-changed)
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
        (make-instance 'fix
                       :issued-by repair
                       :problem problem
                       ;:restart-data item-based-cxn
                       )))))



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
        (notify lexicon-changed)
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
        (make-instance 'fix
                       :issued-by repair
                       :problem problem
                       ;:restart-data lex-cxn
                       )))))



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
        (make-instance 'fix
                       :issued-by repair
                       :problem problem
                       :restart-data t
                       )))))


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
      (make-instance 'fix
                     :issued-by repair
                     :problem problem
                     ;:restart-data holophrase-cxn
                     ))))






(defun get-all-non-duplicate-leaf-nodes (cip)
  (remove-if #'(lambda (node) (find 'fcg::duplicate (fcg::statuses node)))
             (remove nil (traverse-depth-first
                          cip  :collect-fn #'(lambda (node)
                                               (when (null (children node))
                                                 node))))))

(defun get-node-with-matching-slots-and-lexical-cxns (cipns)
  (loop for cipn in cipns
        for applied-cxns = (mapcar #'get-original-cxn
                                   (fcg::applied-constructions cipn))
        for applied-lex-cxns = (find-all 'lexical applied-cxns
                                         :key #'get-cxn-type)
        for applied-item-based-cxn = (find 'item-based applied-cxns
                                           :key #'get-cxn-type)
        when (and applied-lex-cxns applied-item-based-cxn
                  (length= applied-lex-cxns (item-based-number-of-slots applied-item-based-cxn))
                  (null (get-strings-from-root cipn)))
        return (cons applied-cxns cipn)))

(defun get-node-with-one-missing-lex-for-slots (cipns)
  (loop for cipn in cipns
        for applied-cxns = (mapcar #'get-original-cxn
                                   (fcg::applied-constructions cipn))
        for applied-lex-cxns = (find-all 'lexical applied-cxns
                                         :key #'get-cxn-type)
        for applied-item-based-cxn = (find 'item-based applied-cxns
                                           :key #'get-cxn-type)
        when (and applied-item-based-cxn
                  (= (- (item-based-number-of-slots applied-item-based-cxn)
                        (length applied-lex-cxns)) 1)
                  (= (length (get-strings-from-root cipn)) 1))
        return (cons applied-cxns cipn)))

(defun all-applied-cxns (cipn)
  (cond (;initial node
         (and (null (parent cipn))
              (null (children cipn)))
         (values nil nil))
        (;success node
         (find 'fcg::succeeded (fcg::statuses cipn))
         (values (mapcar #'get-original-cxn
                         (fcg::applied-constructions cipn))
                 cipn))
        (t ;otherwise, take all non-duplicate leaf nodes
         (let ((all-leaf-nodes
                (get-all-non-duplicate-leaf-nodes (cip cipn))))
           (if (length= all-leaf-nodes 1)
             ;; if there is only one, return that one
             (values (mapcar #'get-original-cxn
                             (fcg::applied-constructions (first all-leaf-nodes)))
                     (first all-leaf-nodes))
             ;; else, take the deepest one (i.e. most cxns, i.e. most abstract)
             (let* ((node-depth #'(lambda (node) (length (all-parents node))))
                    (deepest-node (the-biggest node-depth all-leaf-nodes)))
               (values (mapcar #'get-original-cxn
                               (fcg::applied-constructions deepest-node))
                       deepest-node)))))))


#|
             ;; else, look at the number of slots of the item-based cxns
             ;; and the applied lexical cxns
             ;; 1. number of slots == number of lexical cxns
             ;; 2. number of slots - number of lexical cxns == 1
             ;; 3. random
             (let* ((matching-slots-and-lexical-cxns
                     (get-node-with-matching-slots-and-lexical-cxns all-leaf-nodes))
                    (one-missing-lex-for-slots
                     (get-node-with-one-missing-lex-for-slots all-leaf-nodes))
                    (random-node
                     (random-elt all-leaf-nodes)))
               (cond (one-missing-lex-for-slots
                      (values (car one-missing-lex-for-slots)
                              (cdr one-missing-lex-for-slots)))
                     (matching-slots-and-lexical-cxns
                      (values (car matching-slots-and-lexical-cxns)
                              (cdr matching-slots-and-lexical-cxns)))
                     (t
                      (values (mapcar #'get-original-cxn
                                      (fcg::applied-constructions random-node))
                              random-node)))))))))
|#
         
  

(defmethod run-process (process
                        (process-label (eql 'parse))
                        task agent)
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
                (irl-program . ,irl-program)
                (restart-occurred-p . ,(find 'restart (status process))))))
           (process-result
            (make-process-result 1 process-result-data :process process)))
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

(defmethod run-process (process
                        (process-label (eql 'determine-success))
                        task agent)
  ;; Check if the computer answer matches the ground truth answer
  (let ((success (and (find-data (input process) 'found-topic)
                      (equal-entity (find-data (input process) 'found-topic)
                                    (topic agent)))))
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
        (notify lexicon-changed)
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
        (notify lexicon-changed)
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
        (notify lexicon-changed)
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

  

