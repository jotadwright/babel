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
  (let* ((hide-type-hierarchy
          (get-configuration experiment :hide-type-hierarchy))
         (learner
          (make-instance 'clevr-learning-learner :role 'learner :experiment experiment
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
          (loop for repair in '(repair-item-based->lexical
                                repair-lexical->item-based
                                repair-add-th-links
                                repair-make-holophrase-cxn
                                repair-holophrase->item-based-substitution
                                repair-holophrase->item-based-addition
                                repair-holophrase->item-based-deletion)
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

(defclass diagnose-parsing-result (diagnostic)
  ((trigger :initform 'parsing-finished))
  (:documentation "Diagnostic to check the result of parsing"))

(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails and there are no applied cxns"))

(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails and there are some applied cxns"))

(define-event parsing-succeeded)


;; It is possible that FCG has tried multiple branches
;; and none of them lead to a solution.
;; However, combining the cxns applied in these branches
;; can allow us to learn something...
;; There are several cases:
;; 1. permutations
;;    Applying X-cxn and Y-cxn in branch 1
;;    Applying Y-cxn and X-cxn in branch 2
;;    This can be detected through 'permutation-of?'
;;    Here, we only want to consider the cxns of one of the branches
;; 2. competitors
;;    Applying the-X-is-what-color-1-cxn and the-X-is-what-color-2-cxn
;;    This can be detected through the form of the cxns
;;    Here, we want to consider just one of them?
;; (3. competitors and permutations can occur together?)
;; 4. none of the above
;;    the-big-X-is-what-color in branch 1
;;    big-cxn in branch 2
;;    Here, we want to comine the applied cxns of the branches
;;    to learn something?

(defun all-applied-cxns (cipn)
  (let ((leaf-nodes
         (remove nil
                 (traverse-depth-first
                  (cip cipn) :collect-fn #'(lambda (node)
                                             (when (null (children node))
                                               node))))))
    (if (length= leaf-nodes 1)
      (mapcar #'get-original-cxn
              (fcg::applied-constructions (first leaf-nodes)))
      (let* ((applied-cxns-per-branch
              (loop for node in leaf-nodes
                    collect (mapcar #'get-original-cxn
                                    (fcg::applied-constructions node))))
             (applied-cxns (first applied-cxns-per-branch))
             (other-branches-identical-p
              (loop for other-cxns in (rest applied-cxns-per-branch)
                    always (permutation-of? other-cxns applied-cxns))))
        (if other-branches-identical-p
          applied-cxns
          (apply #'append applied-cxns-per-branch))))))
          
  

(defmethod diagnose ((diagnostic diagnose-parsing-result)
                     process-result &key trigger)
  ;; check if parsing succeeded
  ;; a different repair is triggered depending on
  ;; whether no cxns could apply
  ;; or some cxns could apply
  (declare (ignorable trigger))
  (cond ((find 'fcg::succeeded (fcg:statuses (get-data process-result 'cipn)))
         (notify parsing-succeeded))
        ((null (get-data process-result 'applied-cxns))
         (make-instance 'unknown-utterance-problem))
        ((not (null (get-data process-result 'applied-cxns)))
         (set-data process-result 'applied-cxns
                   (all-applied-cxns (get-data process-result 'cipn)))
         (make-instance 'partial-utterance-problem))))


(defclass repair-make-holophrase-cxn (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair uses the composer to create a new program
                   for the current question."))

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
  (let* ((agent (owner (task (process object))))
         (holophrase-cxn
          (run-repair agent (get-data object 'applied-cxns)
                      (get-data object 'cipn) :add-holophrase)))
    (add-cxn holophrase-cxn (grammar agent))
    (notify add-holophrase-new-cxn holophrase-cxn)
    (make-instance 'fix :issued-by repair :problem problem)))

(defmethod repair ((repair repair-make-holophrase-cxn)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new holophrase cxn and adding it to
  ;; the agent's grammar
  (declare (ignorable trigger))
  (notify add-holophrase-repair-started)
  (let* ((agent (owner (task (process object))))
         (holophrase-cxn
          (run-repair agent (get-data object 'applied-cxns)
                      (get-data object 'cipn) :add-holophrase)))
    (add-cxn holophrase-cxn (grammar agent))
    (notify add-holophrase-new-cxn holophrase-cxn)
    (make-instance 'fix :issued-by repair :problem problem)))



(defclass repair-lexical->item-based (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to create an item-based cxn,
                   given a lexical cxn"))
                         
(define-event lexical->item-based-repair-started)
(define-event lexical->item-based-new-cxn-and-links
  (cxn construction) (th type-hierarchy))

(defmethod repair ((repair repair-lexical->item-based)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn)))
    ;; only lexical cxns applied -> make an item-based cxn
    (let ((item-based-cxn-and-th-links
           (run-repair agent applied-cxns cipn :lexical->item-based)))
      (when item-based-cxn-and-th-links
        (notify lexical->item-based-repair-started)
        (destructuring-bind (item-based-cxn th-links)
            item-based-cxn-and-th-links
          (add-cxn item-based-cxn (grammar agent))
          ;; th-links are grouped per applied lex cxn
          (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
                for th-group in th-links
                do (loop for th-link in th-group
                         do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
                         do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5)))
          (notify lexical->item-based-new-cxn-and-links
                  item-based-cxn (get-type-hierarchy (grammar agent)))
          (make-instance 'fix :issued-by repair :problem problem))))))



(defclass repair-item-based->lexical (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to create a lexical cxn, given
                   an item-based cxn"))

(define-event item-based->lexical-repair-started)
(define-event item-based->lexical-new-cxn-and-th-links
  (cxn construction) (th type-hierarchy))

(defmethod repair ((repair repair-item-based->lexical)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn)))
    ;;  item-based cxn applied and root not empty -> make a lexical cxn
    (let ((lex-cxn-and-th-links
           (run-repair agent applied-cxns cipn :item-based->lexical)))
      (when lex-cxn-and-th-links
        (notify item-based->lexical-repair-started)
        (destructuring-bind (lex-cxn th-groups) lex-cxn-and-th-links
          (add-cxn lex-cxn (grammar agent))
          (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
                for th-group in th-groups
                do (loop for th-link in th-group
                         do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
                         do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5)))
          (notify item-based->lexical-new-cxn-and-th-links
                  lex-cxn (get-type-hierarchy (grammar agent)))
          (make-instance 'fix :issued-by repair :problem problem))))))



(defclass repair-add-th-links (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair tries to add th links"))

(define-event add-th-links-repair-started)
(define-event add-th-links-new-th-links
  (th type-hierarchy))

(defmethod repair ((repair repair-add-th-links)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; item-based and lexical-cxn without solution
  ;; => add th links!
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn))
         (th-links
          (run-repair agent applied-cxns cipn :add-th-links)))
      (when th-links
        (notify add-th-links-repair-started)
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
        (notify add-th-links-new-th-links (get-type-hierarchy (grammar agent)))
        (make-instance 'fix :issued-by repair :problem problem))))

;; NOTE: The following case is not covered yet:
;; "How big is the large cube?"
;; how-big-is-the-large-X -> no solution
;; big-cxn -> no solution
;; Depending on which branch is executed first,
;; the lexical->item-based or the item-based->lexical
;; repair will be triggered. However, this could be
;; solved with item-based+lexical->item-based repair.
;; In order for this to work, need to check all branches
;; of comprehension, instead of the applied cxns.

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
            `((cipn . ,cipn)
              (applied-cxns . ,(mapcar #'get-original-cxn
                                       (applied-constructions cipn)))
              (irl-program . ,irl-program)))
           (process-result
            (make-process-result 1 process-result-data :process process)))
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
  ;; run the alignment strategy
  (run-alignment agent (input process)
                 (get-configuration agent :alignment-strategy))
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

(defmethod diagnose ((diagnostic diagnose-aligment-result)
                     process-result &key trigger)
  ;; check if parsing succeeded
  ;; a different repair is triggered depending on
  ;; whether no cxns could apply
  ;; or some cxns could apply
  (declare (ignorable trigger))
  (when (find-data process-result 'success)
    (make-instance 'possible-generalisation-problem)))

(defclass repair-holophrase->item-based-substitution (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using substitution."))

(define-event holophrase->item-based-substitution-repair-started)
(define-event holophrase->item-based-subsititution-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(defmethod repair ((repair repair-holophrase->item-based-substitution)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with substitution
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn))
         (subsititution-cxns-and-th-links
          (run-repair agent applied-cxns cipn :holophrase->item-based--substitution)))
    (when subsititution-cxns-and-th-links
      (notify holophrase->item-based-substitution-repair-started)
      (destructuring-bind (cxn-1 cxn-2 cxn-3 &rest th-links)
          subsititution-cxns-and-th-links
        (add-cxn cxn-1 (grammar agent))
        (add-cxn cxn-2 (grammar agent))
        (add-cxn cxn-3 (grammar agent))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
        (notify holophrase->item-based-subsititution-new-cxn-and-th-links
                (list cxn-1 cxn-2 cxn-3)
                (get-type-hierarchy (grammar agent)))
        (make-instance 'fix :issued-by repair :problem problem)))))


(defclass repair-holophrase->item-based-addition (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using addition."))

(define-event holophrase->item-based-addition-repair-started)
(define-event holophrase->item-based-addition-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(defmethod repair ((repair repair-holophrase->item-based-addition)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with addition
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn))
         (addition-cxns-and-th-links
          (run-repair agent applied-cxns cipn :holophrase->item-based--addition)))
    (when addition-cxns-and-th-links
      (notify holophrase->item-based-addition-repair-started)
      (destructuring-bind (lex-cxn item-based-cxn &rest th-links)
          addition-cxns-and-th-links
        (add-cxn lex-cxn (grammar agent))
        (add-cxn item-based-cxn (grammar agent))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
        (notify holophrase->item-based-addition-new-cxn-and-th-links
                (list lex-cxn item-based-cxn)
                (get-type-hierarchy (grammar agent)))
        (make-instance 'fix :issued-by repair :problem problem)))))



(defclass repair-holophrase->item-based-deletion (repair)
  ((trigger :initform 'alignment-finished))
  (:documentation "Repair created when a generalisation is possible.
                   This repair tries to generalise over holophrases using deletion."))

(define-event holophrase->item-based-deletion-repair-started)
(define-event holophrase->item-based-deletion-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(defmethod repair ((repair repair-holophrase->item-based-deletion)
                   (problem possible-generalisation-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  ;; holophrase->item-based with addition
  (let* ((agent (owner (task (process object))))
         (applied-cxns
          (find-data object 'applied-cxns))
         (cipn (find-data object 'cipn))
         (deletion-cxns-and-th-links
          (run-repair agent applied-cxns cipn :holophrase->item-based--deletion)))
    (when deletion-cxns-and-th-links
      (notify holophrase->item-based-deletion-repair-started)
      (destructuring-bind (lex-cxn item-based-cxn &rest th-links)
          deletion-cxns-and-th-links
        (when lex-cxn
          (add-cxn lex-cxn (grammar agent)))
        (add-cxn item-based-cxn (grammar agent))
        (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
              for th-link in th-links
              do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
              do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
        (notify holophrase->item-based-deletion-new-cxn-and-th-links
                (list lex-cxn item-based-cxn)
                (get-type-hierarchy (grammar agent)))
        (make-instance 'fix :issued-by repair :problem problem)))))

  

