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
                              :diagnostics (list (make-instance 'diagnose-parsing-result))
                              :repairs (list (make-instance 'repair-make-holophrase-cxn)
                                             (make-instance 'repair-add-th-link-or-cxns))))
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
  () (:documentation "Problem created when parsing fails and there are no applied cxns"))

(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when parsing fails and there are some applied cxns"))

(defclass diagnose-parsing-result (diagnostic)
  ((trigger :initform 'parsing-finished))
  (:documentation "Diagnostic to check the result of parsing"))

(defclass repair-make-holophrase-cxn (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair uses the composer to create a new program
                   for the current question."))

(defclass repair-add-th-link-or-cxns (repair)
  ((trigger :initform 'parsing-finished))
  (:documentation "Repair created when a parsing problem is diagnosed.
                   This repair adds links to the th in order to solve the parse."))

(define-event parsing-succeeded)

(defmethod diagnose ((diagnostic diagnose-parsing-result)
                     process-result &key trigger)
  ;; check if parsing succeeded
  ;; a different repair is triggered depending on
  ;; whether no cxns could apply
  ;; or some cxns could apply
  (declare (ignorable trigger))
  (cond ((get-data process-result 'parsing-succeeded)
         (notify parsing-succeeded))
        ((null (get-data process-result 'applied-cxns))
         (make-instance 'unknown-utterance-problem))
        ((not (null (get-data process-result 'applied-cxns)))
         (make-instance 'partial-utterance-problem))))


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
    (notify add-holophrase-new-cxn holophrase-cxn)))


(define-event item-based->lexical-repair-started)
(define-event item-based->lexical-new-cxn-and-th-links
  (cxn construction) (th type-hierarchy))

(define-event lexical->item-based-repair-started)
(define-event lexical->item-based-new-cxn-and-links
  (cxn construction) (th type-hierarchy))

;; TO DO; refactor this such that all repairs are tried
;; but the checks are implemented at the repair level.
;; It should fall through all repairs until one works
;; or otherwise, a holophrase is added.

;; 1. only lexical cxns with good scores -> lexical->item-based
;; 2. item-based (possibly with lexical) and root not empty -> item-based->lexical
;; 3. item-based and with lexical and root empty -> add-th-links or compose update program for item-based and/or lex??
;; 4. no applied cxns or none of the above -> compose-new-program

(defmethod repair ((repair repair-add-th-link-or-cxns)
                   (problem partial-utterance-problem)
                   (object process-result)
                   &key trigger)
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (applied-cxns (get-data object 'applied-cxns))
         (cipn (get-data object 'cipn))
         (only-lexical-cxns
          (loop for cxn in applied-cxns
                always (eql (attr-val cxn :cxn-type) 'lexical))))
    (if only-lexical-cxns
      ;; if the partial program only contains bind statements
      ;; use it to constrain the composer
      ;; and make an item-based cxn + th links from the result
      ;; To improve the quality of these results, we can
      ;; empose a threshold on the score of the lexical cxn.
      ;; When the threshold is not met, simply create a new
      ;; holophrase from scratch.
      (let ((lex-cxns-any-good-p
             (loop for cxn in applied-cxns
                   always (> (attr-val cxn :score) 0.6))))
        
        (if lex-cxns-any-good-p
          (let ((item-based-cxn-and-th-links
                 (run-repair agent applied-cxns cipn :lexical->item-based)))
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
                      item-based-cxn (get-type-hierarchy (grammar agent)))))
          
          (progn (notify add-holophrase-repair-started)
            (let ((holophrase-cxn
                   (run-repair agent applied-cxns cipn :add-holophrase)))
              (add-cxn holophrase-cxn (grammar agent))
              (notify add-holophrase-new-cxn holophrase-cxn)))))
      
      ;; if not only lexical cxns, the applied cxns can be either
      ;; an item-based cxn or a combination of item-based and lexical
      ;; that does not yield a solution. In both of these cases, if there
      ;; are still strings in the root, there is a lexical cxn missing
      ;; to parse this utterance. This can be fixed using the
      ;; item-based->lexical repair.
      ;; If the root is empty, the parse could have failed due to missing
      ;; th links. We try to add these.
      ;; ??? Is it okay to check the root?
      (let ((strings-in-root
             (gl::form-predicates-with-variables
              (extract-strings
               (get-root
                (left-pole-structure
                 (car-resulting-cfs
                  (first (cipn-car cipn)))))))))
        
        (if strings-in-root
          (let ((lex-cxn-and-th-links
                 (run-repair agent applied-cxns cipn :item-based->lexical)))
            (notify item-based->lexical-repair-started)
            (when lex-cxn-and-th-links
              (destructuring-bind (lex-cxn &rest th-links) lex-cxn-and-th-links
                (add-cxn lex-cxn (grammar agent))
                (loop with type-hierarchy = (get-type-hierarchy (grammar agent))
                      for th-link in th-links
                      do (add-categories (list (car th-link) (cdr th-link)) type-hierarchy)
                      do (add-link (car th-link) (cdr th-link) type-hierarchy :weight 0.5))
                (notify item-based->lexical-new-cxn-and-th-links
                        lex-cxn (get-type-hierarchy (grammar agent))))))
          
          nil)))))
  
                                                

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
              (irl-program . ,irl-program)
              (parsing-succeeded . ,(eql (first (statuses cipn)) 'fcg::succeeded))))
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

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))
(define-event adjust-program-started)


;; TO DO; use the add-holophrase repair here

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

;;;; minimal-holophrases+lateral-inhibition (alignment strategy)
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
    (if success
      (if (and (length= applied-cxns 1)
               (eql (attr-val (first applied-cxns) :cxn-type) 'holophrase))
        (progn
          (inc-cxn-score (first applied-cxns) :delta (get-configuration agent :cxn-incf-score))
          (notify cxns-rewarded applied-cxns))
        (progn
          (loop for cxn in applied-cxns
                do (inc-cxn-score cxn :delta (get-configuration agent :cxn-incf-score)))
          (notify cxns-rewarded applied-cxns)
          (loop for competitor in (remove-duplicates
                                   (append (get-meaning-competitors agent applied-cxns)
                                           (get-form-competitors agent applied-cxns)))
                do (dec-cxn-score agent competitor
                                  :delta (get-configuration agent :cxn-decf-score))
                collect competitor into punished-cxns
                finally (notify cxns-punished punished-cxns))))
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

(define-event holophrase->item-based-substitution-repair-started)
(define-event holophrase->item-based-subsititution-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(define-event holophrase->item-based-addition-repair-started)
(define-event holophrase->item-based-addition-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(define-event holophrase->item-based-deletion-repair-started)
(define-event holophrase->item-based-deletion-new-cxn-and-th-links
  (new-cxns list) (th type-hierarchy))

(defmethod run-process (process
                        (process-label (eql 'generalise))
                        task agent)
  ;; try out different generalisation strategies (Jonas' repairs)
  ;;
  ;; NOTE: these repairs are only ran when parsing was a success.
  ;; Other repairs can be executed when parsing failed (see above).
  ;;
  ;; NOTE: in order to learn useful things, we only allow generalisation
  ;; when both cxns involved have a high score.
  (let ((applied-cxns
         (if (find-data (input process) 'new-cxns)
           (find-data (input process) 'new-cxns)
           (find-data (input process) 'applied-cxns)))
        (cipn (find-data (input process) 'cipn)))
    ;; 2. holophrase->item-based with substitution
    (let ((subsititution-cxns-and-th-links
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
                  (get-type-hierarchy (grammar agent))))))
    ;; 3. holophrase->item-based with addition
    (let ((addition-cxns-and-th-links
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
                  (get-type-hierarchy (grammar agent))))))
    ;; 4. holophrase->item-based with deletion
    (let ((deletion-cxns-and-th-links
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
                  (get-type-hierarchy (grammar agent)))))))
    ;; process results
    (make-process-result 1 nil :process process))


  

