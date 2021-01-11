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
                                           align
                                           generalise)
                              :diagnostics (list (make-instance 'diagnose-unknown-utterance))
                              :repairs (list (make-instance 'repair-make-holophrase-cxn))))
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

(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails"))

(defclass diagnose-unknown-utterance (diagnostic)
  ((trigger :initform 'parsing-finished))
  (:documentation "Diagnostic to check the result of parsing"))

(defclass repair-make-holophrase-cxn (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair uses the composer to create a new program
                   for the current question."))

(define-event parsing-succeeded)
(define-event new-program-repair-started)
(define-event chosen-composer-solution (solution chunk-evaluation-result))

(defmethod diagnose ((diagnostic diagnose-unknown-utterance)
                     process-result &key trigger)
  ;; check if parsing succeeded
  (declare (ignorable trigger))
  (if (get-data process-result 'parsing-succeeded)
    (notify parsing-succeeded)
    (make-instance 'unknown-utterance-problem)))

(defmethod repair ((repair repair-make-holophrase-cxn)
                   (problem unknown-utterance-problem)
                   (object process-result)
                   &key trigger)
  ;; repair by composing a new program,
  ;; making a new holophrase cxn and adding it to
  ;; the agent's grammar
  ;;
  ;; TO DO; the composer can make use of a partial
  ;; program, if there is any.
  ;; However, this can be completely wrong, so maybe
  ;; only make use of high quality partial programs
  ;; (i.e. cxns with high scores)
  ;;
  ;; TO DO; the new cxn does not have to be a holophrase
  ;; cxn per se. If there was a partial program, the agent
  ;; could immediately make an item-based cxn
  (declare (ignorable trigger))
  (notify new-program-repair-started)
  (let* ((agent (owner (task (process object))))
         (composer-solution (compose-new-program agent (topic agent)))
         (new-irl-program (append (bind-statements composer-solution)
                                  (irl-program (chunk composer-solution)))))
    (notify chosen-composer-solution composer-solution)
    (add-holophrase-cxn (grammar agent) (utterance agent) new-irl-program
                        :initial-score (get-configuration agent :initial-cxn-score))
    (add-composer-chunk agent (chunk (irl::node composer-solution)))))
                                                

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
            `((applied-cxns . ,(mapcar #'get-original-cxn
                                       (applied-constructions cipn)))
              (irl-program . ,irl-program)
              (parsing-succeeded . ,(eql (first (statuses cipn))
                                         'fcg::succeeded))))
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
  (let (process-result-data)
    (when (find-data (input process) 'irl-program)
       (let* ((irl-program (find-data (input process) 'irl-program))
              (all-solutions
               (evaluate-irl-program irl-program (ontology agent)
                                     :primitive-inventory (available-primitives agent))))
         ;; there should only be a single solution
         (when (length= all-solutions 1)
           (let ((answer (get-target-value irl-program (first all-solutions))))
             (push (cons 'found-topic answer) process-result-data)
             (notify interpretation-succeeded answer)))))
    (make-process-result 1 process-result-data :process process)))

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
  (let (
        ;; run the alignment strategy, possibly yielding
        ;; some new cxns
        (new-cxns (run-alignment agent (input process)
                                 (get-configuration agent :alignment-strategy)))
        )
    ;; create a process result with these new cxns
    (make-process-result 1 `((new-cxns . ,(cond ((null new-cxns) nil)
                                                ((listp new-cxns) new-cxns)
                                                (t (list new-cxns)))))
                         :process process)))

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))
(define-event adjust-program-started)

(defun add-past-program (agent irl-program)
  (when irl-program
    (if (find-data agent 'past-programs)
      (if (assoc (utterance agent) (get-data agent 'past-programs) :test #'string=)
        (push irl-program
              (cdr (assoc (utterance agent)
                          (get-data agent 'past-programs)
                          :test #'string=)))
        (push-data agent 'past-programs 
                   (cons (utterance agent) (list irl-program))))         
      (set-data agent 'past-programs
                (list (cons (utterance agent) (list irl-program)))))))

(defun add-past-scene (agent)
  (let* ((clevr-scene (find-data (ontology agent) 'clevr-context))
         (new-sample (list (index clevr-scene) (utterance agent) (topic agent))))
    (unless (find new-sample (find-data agent 'past-scenes)
                  :test #'(lambda (s1 s2)
                            (and (= (first s1) (first s2))
                                 (string= (second s1) (second s2)))))
      (push-data agent 'past-scenes new-sample))))


;;;; store past programs (composer strategy)
;;;; ==> store all past programs and use these when composing a new program
;;;;     (the new program must be different from all previous programs).

(defmethod alignment-update-program (agent process-input
                                     (composer-strategy (eql :store-past-programs)))
  (let ((success (find-data process-input 'success)))
    ;; when no success
    (unless success
      ;; add the current program to the list of past programs for this utterance
      (add-past-program agent (find-data process-input 'irl-program))
      ;; compose a new holophrase cxn
      (notify adjust-program-started)
      (let* ((composer-solution (compose-program-update agent (topic agent) composer-strategy))
             (new-irl-program (append (bind-statements composer-solution)
                                      (irl-program (chunk composer-solution)))))
        (notify chosen-composer-solution composer-solution)
        (add-composer-chunk agent (chunk (irl::node composer-solution)))
        (add-holophrase-cxn (grammar agent) (utterance agent) new-irl-program
                            :initial-score (get-configuration agent :initial-cxn-score))))))

;;;; store past scenes (composer strategy)
;;;; ==> store all past scenes with the applied utterance and correct answer
;;;;     and use these when composing a new program (the new program must
;;;;     lead to the correct answer in all of these scenes).

(defmethod alignment-update-program (agent process-input
                                     (composer-strategy (eql :store-past-scenes)))
  (let ((success (find-data process-input 'success)))
    ;; store the scene
    (add-past-scene agent)
    (unless success
      ;; compose a new holophrase cxn
      (notify adjust-program-started)
      (let* ((composer-solution (compose-program-update agent (topic agent) composer-strategy))
             (new-irl-program (append (bind-statements composer-solution)
                                      (irl-program (chunk composer-solution)))))
        (notify chosen-composer-solution composer-solution)
        (add-composer-chunk agent (chunk (irl::node composer-solution)))
        (add-holophrase-cxn (grammar agent) (utterance agent) new-irl-program
                            :initial-score (get-configuration agent :initial-cxn-score))))))

;;;; minimal (alignment strategy)
;;;; ==> Only keep a single cxn for each utterance, so delete the previous one.

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input
                          (strategy (eql :minimal)))
  (let ((success (find-data process-input 'success)))
    (notify alignment-started)
    ;; when unsuccessful ...
    (unless success
      ;; remove the cxn for this utterance
      ;; but only if it was a holophrase cxn
      ;; also need to remove item-based cxns?
      (when (find-data process-input 'applied-cxns)
        (loop for cxn in (find-data process-input 'applied-cxns)
              when (eql (attr-val cxn :cxn-type) 'holophrase)
              do (delete-cxn cxn (grammar agent)))))
    (alignment-update-program agent process-input (get-configuration agent :composer-strategy))))

;;;; lateral-inhibition (alignment strategy)
;;;; ==> keep all cxns for all utterances, but reward them using lateral
;;;;     inhibition.

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input
                          (strategy (eql :lateral-inhibition)))
  (let ((success (find-data process-input 'success)))
    (notify alignment-started)
    ;; reward the applied cxns and punish competitors
    ;; or punish the applied cxns in case of failure
    (if success
      (progn
        (loop for cxn in (find-data process-input 'applied-cxns)
              do (inc-cxn-score cxn :delta (get-configuration agent :cxn-incf-score)))
        (notify cxns-rewarded (find-data process-input 'applied-cxns))
        (loop with applied-cxns = (find-data process-input 'applied-cxns)
              for competitor in (remove-duplicates
                                 (append (get-meaning-competitors agent applied-cxns)
                                         (get-form-competitors agent applied-cxns)))
              do (dec-cxn-score agent competitor
                                :delta (get-configuration agent :cxn-decf-score))
              collect competitor into punished-cxns
              finally (notify cxns-punished punished-cxns)))
      (when (find-data process-input 'applied-cxns)
        (loop for cxn in (find-data process-input 'applied-cxns)
              do (dec-cxn-score agent cxn :delta (get-configuration agent :cxn-decf-score)))
        (notify cxns-punished (applied-cxns agent))))
    (alignment-update-program agent process-input (get-configuration agent :composer-strategy))))

;;;; :minimal-holophrases+lateral-inhibition (alignment strategy)
;;;; ==> we only want to keep a single cxn for each holophrase since
;;;;     we only want to be generalising over the last/best holophrase
;;;;     cxns. Therefore, we use the minimal strategy when concerning
;;;;     holophrases and use the lateral inhibition strategy when
;;;;     concerning the other cxns (e.g. there can be different versions
;;;;     of the same lexical cxns and we need to learn which one is correct)

(defmethod run-alignment ((agent clevr-learning-learner)
                          process-input
                          (strategy (eql :minimal-holophrases+lateral-inhibition)))
  (let ((success (find-data process-input 'success))
        (applied-cxns (find-data process-input 'applied-cxns)))
    (notify alignment-started)
    ;; if there is a single applied cxns and it is a holophrase-cxn
    ;; apply the minimal strategy
    ;; otherwise, apply lateral inhibition
    (if success
      (when (length> applied-cxns 1)
        (loop for cxn in applied-cxns
              do (inc-cxn-score cxn :delta (get-configuration agent :cxn-incf-score)))
        (notify cxns-rewarded applied-cxns)
        (loop for competitor in (remove-duplicates
                                 (append (get-meaning-competitors agent applied-cxns)
                                         (get-form-competitors agent applied-cxns)))
              do (dec-cxn-score agent competitor
                                :delta (get-configuration agent :cxn-decf-score))
              collect competitor into punished-cxns
              finally (notify cxns-punished punished-cxns)))
      (if (and (length= applied-cxns 1)
               (eql (attr-val (first applied-cxns) :cxn-type) 'holophrase))
        (delete-cxn (first applied-cxns) (grammar agent))
        (loop for cxn in applied-cxns
              do (dec-cxn-score agent cxn :delta (get-configuration agent :cxn-decf-score))
              finally (notify cxns-punished applied-cxns))))
    (alignment-update-program agent process-input (get-configuration agent :composer-strategy))))
        
           
;; --------------------------
;; + Generalisation process +
;; --------------------------

(defmethod run-process (process
                        (process-label (eql 'generalise))
                        task agent)
  ;; try out different generalisation strategies (Jonas' repairs)
  ;;
  ;; NOTE: only doing this when communication was successful 
  ;; could increase the changes that something useful is being
  ;; learned here. However, for now, we always try to generalise
  ;(when (find-data (input process) 'success)
  (let ((applied-cxns
         (if (find-data (input process) 'new-cxns)
           (find-data (input process) 'new-cxns)
           (find-data (input process) 'applied-cxns))))
    (cond ((and (length= applied-cxns 1)
                (eql (attr-val (first applied-cxns) :cxn-type) 'holophrase))
           ;; a holophrase cxn was applied
           ;; try to learn an item-based cxn
           (let ((subsititution-cxns-and-th-links
                  (create-item-based-cxn agent (first applied-cxns))))
             (unless (null subsititution-cxns-and-th-links)
               (destructuring-bind (cxn-1 cxn-2 cxn-3 &rest th-links)
                   subsititution-cxns-and-th-links
                 (declare (ignorable th-links))
                 (add-cxn cxn-1 (grammar agent))
                 (add-cxn cxn-2 (grammar agent))
                 (add-cxn cxn-3 (grammar agent))
                 (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
                       for th-link in th-links
                       do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
                       do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
                 ))))))
  (make-process-result 1 nil :process process))

(defun create-item-based-cxn (agent applied-holophrase-cxn)
  (let* ((cxn-inventory (grammar agent))
         (utterance
          (list-of-strings->string
           (render
            (extract-form-predicates applied-holophrase-cxn)
            (get-configuration cxn-inventory :render-mode))))
         (meaning
          (extract-meaning-predicates applied-holophrase-cxn)))
    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-cxn
                          overlapping-form-cxn
                          cxn)
        (gl::select-cxn-for-making-item-based-cxn cxn-inventory utterance meaning)
      (when cxn
        (let* ((cxn-name-item-based-cxn
                (gl::make-cxn-name overlapping-form-cxn cxn-inventory
                                   :add-cxn-suffix nil))
               (lex-cxn-1
                (gl::find-cxn-by-form-and-meaning non-overlapping-form-cxn
                                              non-overlapping-meaning-cxn
                                              cxn-inventory))
               (lex-cxn-2
                (gl::find-cxn-by-form-and-meaning non-overlapping-form-observation
                                              non-overlapping-meaning-observation
                                              cxn-inventory))
               ;; unit names
               (unit-name-lex-cxn-1
                (second (find 'string non-overlapping-form-cxn :key #'first)))
               (unit-name-lex-cxn-2
                (second (find 'string non-overlapping-form-observation :key #'first)))
               ;; args and syn-cat
               (lex-class-lex-cxn-1
                (if lex-cxn-1
                  (gl::lex-class-cxn lex-cxn-1)
                  (intern (symbol-name (make-const unit-name-lex-cxn-1)) :type-hierarchies)))
               (lex-class-lex-cxn-2
                (if lex-cxn-2
                  (gl::lex-class-cxn lex-cxn-2)
                  (intern (symbol-name (make-const unit-name-lex-cxn-2)) :type-hierarchies)))
               (lex-class-item-based-cxn
                (intern (symbol-name (make-const cxn-name-item-based-cxn)) :type-hierarchies))
               ;; Type hierachy links
               (th-link-1 (cons lex-class-lex-cxn-1 lex-class-item-based-cxn))
               (th-link-2 (cons lex-class-lex-cxn-2 lex-class-item-based-cxn))
               (th-link-3 (cons (cdr th-link-1) (car th-link-1)))
               (th-link-4 (cons (cdr th-link-2) (car th-link-2)))
               ;; Args
               (args-lex-cxn-1 (third (first non-overlapping-meaning-cxn))) ;; third if bind
               (args-lex-cxn-2 (third (first non-overlapping-meaning-observation))) ;; third if bind
               ;; CXNs
               (new-lex-cxn-1
                (or lex-cxn-1
                    (second
                     (multiple-value-list
                      (eval
                       `(def-fcg-cxn ,(gl::make-cxn-name non-overlapping-form-cxn cxn-inventory)
                                     ((,unit-name-lex-cxn-1
                                       (args (,args-lex-cxn-1))
                                       (syn-cat (phrase-type lexical)
                                                (lex-class ,lex-class-lex-cxn-1)))
                                      <-
                                      (,unit-name-lex-cxn-1
                                       (HASH meaning ,non-overlapping-meaning-cxn)
                                       --
                                       (HASH form ,non-overlapping-form-cxn)))
                                     :attributes (:cxn-type lexical)
                                     :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-lex-cxn-2
                (or lex-cxn-2
                    (second
                     (multiple-value-list
                      (eval
                       `(def-fcg-cxn ,(gl::make-cxn-name non-overlapping-form-observation cxn-inventory)
                                     ((,unit-name-lex-cxn-2
                                       (args (,args-lex-cxn-2))
                                       (syn-cat (phrase-type lexical)
                                                (lex-class ,lex-class-lex-cxn-2)))
                                      <-
                                      (,unit-name-lex-cxn-2
                                       (HASH meaning ,non-overlapping-meaning-observation)
                                       --
                                       (HASH form ,non-overlapping-form-observation)))
                                     :attributes (:cxn-type lexical)
                                     :cxn-inventory ,(copy-object cxn-inventory)))))))
               (item-based-cxn
                (second
                 (multiple-value-list
                  (eval
                   `(def-fcg-cxn ,(gl::add-cxn-suffix cxn-name-item-based-cxn)
                                 ((?item-based-unit
                                   (syn-cat (phrase-type item-based))
                                   (subunits (,unit-name-lex-cxn-1)))
                                  (,unit-name-lex-cxn-1
                                   (args (,args-lex-cxn-1))
                                   (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                  <-
                                  (?item-based-unit
                                   (HASH meaning ,overlapping-meaning-cxn)
                                   --
                                   (HASH form ,overlapping-form-cxn)))
                                 :attributes (:cxn-type item-based)
                                 :cxn-inventory ,(copy-object cxn-inventory)))))))
          (list new-lex-cxn-1 new-lex-cxn-2 item-based-cxn
                th-link-1 th-link-2 th-link-3 th-link-4))))))
  

