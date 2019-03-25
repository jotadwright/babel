(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurations Experiment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lists attributes and values of the objects in the world, together with words expressing them.
(define-configuration-default-value :ontology '((:shape ((square ."vierkant")
                                                         (circle . "cirkel")
                                                         (triangle . "driehoek")
                                                         (rectangle . "rechthoek")))
                                                (:color ((yellow . "geel")
                                                         (red . "rood")
                                                         (blue . "blauw")
                                                         (green . "groen")))
                                                (:size ((small . "klein")
                                                        (large . "groot")
                                                        (tiny . "minuscuul")
                                                        (huge . "reusachtig")))
                                                (:texture ((fluffy . "pluizig")
                                                           (rough . "ruw")
                                                           (smooth . "glad")
                                                           (soft . "zacht")))))
;; Population Size.
(define-configuration-default-value :population-size 3)
;; Scene Size.
(define-configuration-default-value :min-nr-of-objects-in-scene 6)
(define-configuration-default-value :max-nr-of-objects-in-scene 6)
;; World generation
(define-configuration-default-value :redundant-dimensions nil)
#|(define-configuration-default-value :redundant-dimensions '(((:texture . :shape)
                                                            ((soft . square)
                                                             (fluffy . circle)
                                                             (smooth . triangle)
                                                             (rough . rectangle)))))
|#
;; Topic Size.
(define-configuration-default-value :min-nr-of-objects-in-topic 2)
(define-configuration-default-value :max-nr-of-objects-in-topic 2)
;; Syntax Strategy.
;(define-configuration-default-value :strategy :lexical-strategy)
;(define-configuration-default-value :strategy :grouping-strategy)
(define-configuration-default-value :strategy :n-gram-strategy)
;; Alignment Strategy
;(define-configuration-default-value :alignment :no-alignment)
(define-configuration-default-value :alignment :lateral-inhibition)
(define-configuration-default-value :who-aligns? :both)

(define-configuration-default-value :li-incf-score 0.1)
(define-configuration-default-value :li-decf-score 0.2)
(define-configuration-default-value :li-incf-weight 0.2)
(define-configuration-default-value :li-decf-weight 0.1)

(define-configuration-default-value :remove-cxn-on-lower-bound nil)
(define-configuration-default-value :lower-bound 0.0)
;; Tracing
(define-configuration-default-value :trace-every-nth-interaction 1)

;;;;;;;;;;;;;;
;; Interact ;;
;;;;;;;;;;;;;;

(defmethod interact ((experiment syntax-experiment)
                     (interaction interaction) &key)
  "Defines a single interaction/game"
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
  ;; 1) Perceive new scene and choose topic
  (initialize-interaction experiment interaction speaker hearer)
  ;; 2) The speaker conceptualizes the topic into a meaning
  (conceptualize speaker (get-data speaker :topic) (get-data speaker :scene))
  ;; 3) The speaker formulates the meaning
  (run-formulation speaker (get-data speaker :meaning))
  ;; --- The speaker passes on the formulated utterance to the hearer
  (set-data hearer :utterance (get-data speaker :utterance))
  (notify utterance-passed (get-data speaker :utterance))
  ;; 4) The hearer comprehends the utterance
  (run-comprehension hearer (get-data hearer :utterance))
  ;; 5) The hearer interprets the meaning in the scene
  (interpret hearer (get-data hearer :meaning) (get-data hearer :scene))
  ;; 6) Compare intended topic of speaker and interpreted topic of hearer
  ;;    and set communicated-succesfully for speaker, hearer and interaction
  (compare-topics speaker hearer interaction)
  ;; 7) If not succesful, then learning phase for hearer
  (unless (communicated-successfully hearer)
    (run-learning hearer (get-data speaker :topic) (get-data hearer :cipn)))
  ;; 8) Speaker and hearer update the scores of the constructions used and their competitors
  (align-agent speaker (get-configuration experiment :alignment))
  (align-agent hearer (get-configuration experiment :alignment))
  ;; Finishing interaction
  (finish-interaction experiment interaction)))

(defun finish-interaction (experiment interaction)
  (notify finishing-interaction interaction)
  (when (and (get-configuration experiment :trace-every-nth-interaction)
               (or (= (interaction-number interaction) 1) (= 0 (mod (interaction-number interaction)
                                                                    (get-configuration experiment :trace-every-nth-interaction)))))
      (deactivate-monitor trace-interaction)
      (deactivate-monitor trace-fcg))
;(pop (interactions experiment))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Initialize interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-interaction (experiment interaction speaker hearer)
  "Speaker and hearer get a scene and the speaker chooses a topic."
  (let* ((scene (perceive-new-scene (world experiment)
                                   :min (get-configuration experiment :min-nr-of-objects-in-scene)
                                   :max (get-configuration experiment :max-nr-of-objects-in-scene)))
        (topic (select-topic scene
                             :min (get-configuration experiment :min-nr-of-objects-in-topic)
                             :max (get-configuration experiment :max-nr-of-objects-in-topic))))
    (set-data speaker :scene scene)
    (set-data (blackboard (grammar speaker)) :scene scene) ;; For accesssing it from FCG in goal tests
    (set-data speaker :topic topic)
    (set-data hearer :scene scene)
    (set-data (blackboard (grammar hearer)) :scene scene) ;; For accesssing it from FCG in goal tests
    (setf (problems speaker) nil)
    (setf (problems hearer) nil)
    (when (and (get-configuration experiment :trace-every-nth-interaction)
               (or (= (interaction-number interaction) 1) (= 0 (mod (interaction-number interaction)
                                                                    (get-configuration experiment :trace-every-nth-interaction)))))
      (activate-monitor trace-interaction)
      (activate-monitor trace-fcg))
    (notify starting-interaction experiment interaction)
    (notify scene-perceived scene)
    (notify topic-selected topic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Conceptualization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conceptualize (agent topic scene)
  "Takes a topic and a scene and returns an instatiated meaning network that discriminates
   the topic from the scene."
  (let* ((topic-objects (objects topic))
         (scene-objects (objects scene))
         (object-ids (mapcar #'id topic-objects))
         (discriminatory-features-objects (discriminate-objects topic-objects scene-objects))
         (non-discriminatory-features-objects (get-non-discriminatory-features-for-objects
                                               topic-objects discriminatory-features-objects))
         (meaning-predicates (loop for object-id in object-ids
                                   for discriminatory-features-object in discriminatory-features-objects
                                   append (features->predicates object-id discriminatory-features-object)))
         (redundant-meaning-predicates (loop for object-id in object-ids
                                             for non-discriminatory-features-object in non-discriminatory-features-objects
                                             append (features->predicates object-id non-discriminatory-features-object))))
    (dolist (redundant redundant-meaning-predicates)
      (assert (null (find redundant meaning-predicates :test #'equalp))))
    (set-data agent :meaning meaning-predicates)
    (set-data (blackboard (grammar agent)) :redundant-meaning redundant-meaning-predicates) ;;so we can access it in create-initial-structure
    (notify conceptualization-finished topic discriminatory-features-objects meaning-predicates redundant-meaning-predicates)))

(defun features->predicates (object-id features)
  "'((:SIZE . LARGE) (:SHAPE . CIRCLE))
   => ((LARGE 20) (CIRCLE 20))"
  (mapcar #'(lambda (feature)
              (list (cdr feature) object-id)) features))

;;;;;;;;;;;;;;;;;;;;
;; 3. Formulation ;;
;;;;;;;;;;;;;;;;;;;;

(defun run-formulation (agent meaning-predicates)
  "Formulation."
  (multiple-value-bind (utterance cipn)
      (formulate meaning-predicates :cxn-inventory (grammar agent))
    (set-data agent :applied-constructions (mapcar #'get-original-cxn (applied-constructions cipn)))
    (set-data agent :cipn-after-initial-comprehension-or-formulation cipn)
    ;; For Learning
    (if (and (field? (goal-test-data cipn) :number-of-hypotheses-formulation)
             (diagnostics agent))
      (progn
        (set-data agent :problematic-cipn cipn)
        (notify learning-speaker-started)
        (multiple-value-bind (new-problems new-fixes)
            (notify-learning agent :trigger 'multiple-hypotheses-formulation)
          (declare (ignore new-problems))
          (assert (= 1 (length new-fixes)))
          (let ((repaired-cipn (get-data (first new-fixes) :fixed-cars)))
            (if (not (field? (goal-test-data repaired-cipn) :number-of-hypotheses-formulation))
              (progn
                (dolist (cxn (constructions (construction-inventory repaired-cipn)))
                  (unless (find-same-cxn (get-original-cxn cxn) (grammar agent) (get-configuration agent :strategy))
                   (add-cxn (get-original-cxn cxn) (grammar agent))))
                (when (equal (get-configuration agent :strategy) :categorisation-strategy)
                  (setf (type-hierarchies::graph (get-type-hierarchy (grammar agent))) (type-hierarchies::graph (get-type-hierarchy (construction-inventory repaired-cipn))))
                  (notify type-hierarchy-updated (get-type-hierarchy (grammar agent))))
                (set-data agent :cipn repaired-cipn)
                (set-data agent :utterance (render (car-resulting-cfs (cipn-car repaired-cipn))
                                                   (get-configuration (construction-inventory repaired-cipn) :render-mode))))
              (progn
                (set-data agent :utterance utterance)
                (set-data agent :cipn cipn))))))
      (progn
        (set-data agent :cipn cipn)
        (set-data agent :utterance utterance)))))

(in-package :fcg)

(export '(list-of-meanings produce formulate create-initial-structure))

(defmethod formulate (meaning &key (cxn-inventory syntax-grammar) (silent nil))
  "formulate the input meaning in FCG Light"
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory))
        (list-of-meanings (make-instance 'list-of-meanings
                                         :non-redundant-meaning meaning
                                         :redundant-meaning (get-data (blackboard cxn-inventory) :redundant-meaning))))
                                         
    (if processing-cxn-inventory
      (produce list-of-meanings processing-cxn-inventory silent)
      (format nil "Failed. ~a does not contain a processing-cxn-inventory" cxn-inventory))))

(defclass list-of-meanings ()
  ((redundant :type list
              :initform nil :initarg :redundant-meaning :accessor redundant)
   (non-redundant :type list
                  :initform nil :initarg :non-redundant-meaning :accessor non-redundant))
  (:documentation "class to store redundant and non redundant conceptualized meanings"))

(defmethod produce ((list-of-meanings list-of-meanings) (construction-inventory construction-inventory)
                    &optional silent)
  "Produce for origins of syntax experiment with meaning and optionally redundant meaning."
  (let ((initial-cfs  
         (create-initial-structure list-of-meanings
                                   (get-configuration construction-inventory :create-initial-structure-mode))))
    (initialize-transient-structure-blackboard initial-cfs (non-redundant list-of-meanings) construction-inventory '->)
    (unless silent (notify produce-started (non-redundant list-of-meanings) construction-inventory initial-cfs))
    (multiple-value-bind (solution cip)
        (fcg-apply construction-inventory initial-cfs '->)
      (let ((utterance
             (and solution
                  (or (find-data (goal-test-data solution) 'utterance)
                      (render 
                       (car-resulting-cfs (cipn-car solution)) 
                       (get-configuration construction-inventory :render-mode)
                       :node solution)))))
        (unless silent (notify produce-finished utterance))
        (values utterance solution cip)))))

(defmethod create-initial-structure ((list-of-meaning-and-redundant-meaning list-of-meanings)
                                     (mode (eql :root-with-redundant-meaning)))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,(non-redundant list-of-meaning-and-redundant-meaning))
                               (redundant-meaning ,(redundant list-of-meaning-and-redundant-meaning))
			       (sem-cat nil)
                               (form nil)
                               (syn-cat nil)))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;
;; 4. Comprehension ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun run-comprehension (agent utterance)
  "Comprehension."
  (multiple-value-bind (meaning cipn)
      (comprehend utterance :cxn-inventory (grammar agent))
    (set-data agent :applied-constructions (mapcar #'get-original-cxn (applied-constructions cipn)))
    (set-data agent :cipn-after-initial-comprehension-or-formulation cipn)
    (set-data agent :cipn cipn)
    (set-data agent :meaning meaning)))

;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Interpretation ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret (agent meaning scene)
  "Set possible topics for meaning in scene."
  (let ((possible-topics (get-possible-topics-for-meaning-in-world meaning scene)))
    (set-data agent :possible-topics possible-topics)
    (if (= 1 (length possible-topics))
      (set-data agent :topic (first possible-topics))
      (set-data agent :topic (make-instance 'syntax-topic :objects nil)))
    (notify interpretation-finished possible-topics)))


(defun get-possible-topics-for-meaning-in-world (meaning scene)
  "Returns possible topics for meaning in scene."
  (if (<= (second (multiple-value-list (connected-semantic-network meaning)))
          (+ 1 (get-configuration (experiment scene) :max-nr-of-objects-in-topic)))
    (let* ((scene-objects (objects scene))
           (bindings (unify (cons '== meaning) (syntax-objects->meaning-network scene-objects)))
           (possible-topics nil))
      (cond ((<= (length bindings) 5)
             (loop for binding in bindings
                   do
                   (let* ((topic-object-ids (mapcar #'cdr binding))
                          (topic-objects (mapcar #'(lambda (id)
                                                     (find id scene-objects :key #'id))
                                                 topic-object-ids)))
                     (setf possible-topics (append (list (make-instance 'syntax-topic :objects topic-objects)) possible-topics))))
             (setf possible-topics (mapcar #'remove-duplicate-objects possible-topics)))
            (t
             (setf possible-topics "More than five sets of bindings, not visualising for effciency reasons.")))
      possible-topics)
    "Too many chunks in the meaning network. Not even trying."))

(defun syntax-objects->meaning-network (list-of-syntax-objects)
  "Takes a list of syntax object and returns a semantic network with their attributes."
  (mappend #'syntax-object->meaning-network list-of-syntax-objects))

(defun syntax-object->meaning-network (syntax-object)
  "Takes a list of syntax object and returns a semantic network with its attributes."
    (features->predicates (id syntax-object) (attributes syntax-object)))

(defun remove-duplicate-objects (set-of-syntax-objects)
  "Removes dupblicate objects from world, scene or topic."
  (setf (objects set-of-syntax-objects)
        (remove-duplicates (objects set-of-syntax-objects) :key #'id))
  set-of-syntax-objects)
  

;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Compare Topics ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-topics (speaker hearer interaction)
  "Checks wheter the objects in the topic of the hearer is a permutation
of the objects in the topic of the speaker and sets communicated-successfully accordingly."
  (if (permutation-of? (objects (get-data speaker :topic))
                       (objects (get-data hearer :topic))
                       :key #'id)
    (setf (communicated-successfully speaker) t
          (communicated-successfully hearer) t
          (communicated-successfully interaction) t)
    (setf (communicated-successfully speaker) nil
          (communicated-successfully hearer) nil
          (communicated-successfully interaction) nil))
    (notify success-determined (communicated-successfully hearer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Learning for Hearer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-learning (agent topic cipn)
  ;; set first correct topic
  (set-data agent :topic topic)
  (if (and (field? (goal-test-data cipn) :number-of-hypotheses-comprehension)
           (diagnostics agent))
      (progn
        (notify learning-hearer-started)
        (set-data agent :problematic-cipn cipn)
        (multiple-value-bind (new-problems new-fixes)
            (notify-learning agent :trigger 'multiple-hypotheses-comprehension)
          (declare (ignore new-problems))
          (assert (= 1 (length new-fixes)))
          (let ((repaired-cipn (get-data (first new-fixes) :fixed-cars)))
            (if (not (field? (goal-test-data repaired-cipn) :number-of-hypotheses-comprehension))
              (progn
                (dolist (cxn (constructions (construction-inventory repaired-cipn)))
                  (unless (find-same-cxn (get-original-cxn cxn) (grammar agent) (get-configuration agent :strategy))
                   (add-cxn (get-original-cxn cxn) (grammar agent))))
                (when (equal (get-configuration agent :strategy) :categorisation-strategy)
                  (setf (type-hierarchies::graph (get-type-hierarchy (grammar agent))) (type-hierarchies::graph (get-type-hierarchy (construction-inventory repaired-cipn))))
                  (notify type-hierarchy-updated (get-type-hierarchy (grammar agent))))
                (set-data agent :cipn repaired-cipn))
              (set-data agent :cipn cipn)))))
      (set-data agent :cipn cipn)))

;;;;;;;;;;;;;;;;;;
;; 8. Alignment ;;
;;;;;;;;;;;;;;;;;;

(defgeneric align-agent (agent strategy)
  (:documentation "Can implement different alignment strategies
  depending on the mode."))

(defmethod align-agent :around (agent strategy)
  (declare (ignore strategy))
  (case (get-configuration agent :who-aligns?)
    (:both (call-next-method))
    (:speaker (when (equal (discourse-role agent) 'speaker) (call-next-method)))
    (:hearer (when (equal (discourse-role agent) 'hearer) (call-next-method)))))

(defmethod align-agent ((agent syntax-agent) (strategy (eql :no-alignment)))
  "No alignment.")

(defmethod align-agent ((agent syntax-agent) (strategy (eql :lateral-inhibition)))
  "Lateral inhibition alignment strategy."
  (notify alignment-started agent)
  (if (communicated-successfully agent)
    ;; Communicative Success: loop through applied cxns
    (loop for applied-cxn in (remove-lexical-constructions (get-data agent :applied-constructions))
          do
          ;; Reward the constructions used
          (inc-score applied-cxn (get-configuration agent :li-incf-score))
          (notify construction-rewarded agent applied-cxn)
          ;; Loop through competitors and punish them
          (loop for competing-cxn in (competing-cxns applied-cxn (grammar agent) (get-configuration agent :strategy))
                do
                (dec-score competing-cxn agent
                           :delta (get-configuration agent :li-decf-score)
                           :lower-bound (get-configuration agent :lower-bound)
                           :remove-on-lower-bound (get-configuration agent :remove-cxn-on-lower-bound))
                (notify construction-punished agent competing-cxn)))
    ;; Communicative Failure: 
    (when (equal (discourse-role agent) 'speaker)
      ;; If speaker, loop through applied-cxns and punish them.
      (loop for applied-cxn in (remove-lexical-constructions (get-data agent :applied-constructions))
            do
            (dec-score applied-cxn agent
                       :delta (get-configuration agent :li-decf-score)
                       :lower-bound (get-configuration agent :lower-bound)
                       :remove-on-lower-bound (get-configuration agent :remove-cxn-on-lower-bound))
            (notify construction-punished agent applied-cxn)))))

(defmethod align-agent ((agent syntax-agent) (strategy (eql :lateral-inhibition-markers)))
  "Lateral inhibition alignment strategy for markers (do not decrease score of generic marker cxn!."
  (notify alignment-started agent)
  (if (communicated-successfully agent)
    ;; Communicative Success: loop through applied cxns
    (loop for applied-cxn in (remove 'generic-marker-cxn
                                     (remove-lexical-constructions (get-data agent :applied-constructions))
                                     :key #'name :test #'equalp)
          do
          ;; Reward the constructions used
          (inc-score applied-cxn (get-configuration agent :li-incf-score))
          (notify construction-rewarded agent applied-cxn)
          ;; Loop through competitors and punish them
          (set-data (blackboard (grammar agent)) :cipn (get-data agent :cipn))
          (loop for competing-cxn in (competing-cxns applied-cxn (grammar agent) (get-configuration agent :strategy))
                do
                (dec-score competing-cxn agent
                           :delta (get-configuration agent :li-decf-score)
                           :lower-bound (get-configuration agent :lower-bound)
                           :remove-on-lower-bound (get-configuration agent :remove-cxn-on-lower-bound))
                (notify construction-punished agent competing-cxn)))
    ;; Communicative Failure: 
    (when (equal (discourse-role agent) 'speaker)
      ;; If speaker, loop through applied-cxns and punish them.
      (unless (problems agent)
        (loop for applied-cxn in (remove 'generic-marker-cxn
                                         (remove-lexical-constructions (get-data agent :applied-constructions))
                                         :key #'name :test #'equalp)
              do
              (dec-score applied-cxn agent
                         :delta (get-configuration agent :li-decf-score)
                         :lower-bound (get-configuration agent :lower-bound)
                         :remove-on-lower-bound (get-configuration agent :remove-cxn-on-lower-bound))
              (notify construction-punished agent applied-cxn))))))


(defmethod align-agent ((agent syntax-agent) (strategy (eql :type-hierarchy)))
  "Alignment on type hierarchies."
  (notify alignment-started agent)
  (if (communicated-successfully agent)
    ;; Communicative Success: loop through used type-hierarchy links
    (progn
      (loop for (lex-cat . gramm-cat) in (used-th-links (get-data agent :cipn))
            do
            ;; Reward the type-hierarchy-links used
            (reward-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) (get-configuration agent :li-decf-weight))
            finally (notify type-hierarchy-rewarded agent (used-th-links (get-data agent :cipn))))
      ;; Loop through competitors and punish them
      (loop for (lex-cat . gramm-cat) in (competing-th-links (get-data agent :cipn))
            do
            ;; Reward the type-hierarchy-links used
            (punish-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) (get-configuration agent :li-incf-weight))
            finally (notify type-hierarchy-punished agent (competing-th-links (get-data agent :cipn)))))
    
    ;; Communicative Failure: 
    (when (equal (discourse-role agent) 'speaker)
      ;; If speaker, loop through the used th-links and punish them.
      (loop for (lex-cat . gramm-cat) in (used-th-links (get-data agent :cipn))
            do
            (punish-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) (get-configuration agent :li-incf-weight))
            finally (notify type-hierarchy-punished agent (used-th-links (get-data agent :cipn)))))))

(defun reward-link (lex-cat gramm-cat type-hierarchy &optional delta (lower-bound 0.01))
  (let* ((current-weight (link-weight lex-cat gramm-cat type-hierarchy))
         (new-weight (- current-weight delta)))
    (set-link-weight lex-cat gramm-cat type-hierarchy (max lower-bound new-weight))))

(defun punish-link (lex-cat gramm-cat type-hierarchy &optional delta (upper-bound 1.0))
  (let* ((current-weight (link-weight lex-cat gramm-cat type-hierarchy))
         (new-weight (+ current-weight delta)))
    (set-link-weight lex-cat gramm-cat type-hierarchy (min upper-bound new-weight))))
   
(defun inc-score (cxn &optional (delta 0.1) (upper-bound 1.0))
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-score (cxn agent &key (delta 0.1) (lower-bound 0.0) 
                      (remove-on-lower-bound t))
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (progn
        (assert (eq (type-of cxn) 'fcg-construction))
        (delete-cxn cxn (grammar agent))
        (assert (= (length (constructions (grammar agent)))
                   (length (constructions (processing-cxn-inventory (grammar agent)))))))
      
      (setf (attr-val cxn :score) lower-bound)))
  cxn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing the Lexicon ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((experiment syntax-experiment) &key &allow-other-keys)
  "Creates the population of the experiment, each agent having grammar with lexical constructions
   and creates the world."
  (let* ((ontology (get-configuration experiment :ontology))
         (word-list (mappend #'cadr ontology)))
    ;; Set population
    (setf (population experiment)
          (loop for i from 1 to (get-configuration experiment :population-size)
                collect (make-instance 'syntax-agent
                                       :id i :grammar (initialize-lexicon word-list (get-configuration experiment :strategy))
                                       :experiment experiment)))
    ;; Set world
    (setf (world experiment)
          (make-instance 'syntax-world
                         :experiment experiment
                         :attributes (get-attributes-values-ontology ontology)))))

(defun add-words (cxn-inventory word-list)
  "Adds lexical cxns for word-list to cxn-inventory."
  (loop for (meaning . form) in word-list
        for cxn-name = (make-symbol (string-append (upcase form) "-CXN"))
        for unit-name = (make-var (string-append (upcase form) "-UNIT"))
        do
        (eval `(def-fcg-cxn ,cxn-name
                     ((,unit-name
                       (args (?x))
                       (unit-type word)
                       (syn-cat (lex-class ,(intern (symbol-name (make-const meaning)) :type-hierarchies))))
                      <-
                      (,unit-name
                       (HASH meaning ((,meaning ?x)))
                       --
                       (HASH form ((string ,unit-name ,form)))))
                     :cxn-set lex
                     :cxn-inventory ',cxn-inventory
                     :attributes (:cxn-type lexical-cxn :apply-sequentially t :meaning ,meaning :form ,form)))
        finally (return cxn-inventory)))

(defmethod initialize-instance :after ((agent syntax-agent) &key &allow-other-keys)
  "Sets diagnostics and repairs in the agents, according to strategy."
  (set-diagnostics-and-repairs agent (get-configuration agent :strategy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializing the World ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-attributes-values-ontology (ontology)
  "Returns list of attriubtes and their values in ontology.
    ((:shapes ((square .\"vierkant\") (circle . \"cirkel\"))) (:colors ((yellow . \"geel\")(red . \"rood\"))))
     => ((:SHAPES (SQUARE CIRCLE)) (:COLORS (YELLOW RED)))"
  (mapcar #'get-attributes-values-ontology-attribute ontology))

(defun get-attributes-values-ontology-attribute (ontology-attribute)
  "(:SHAPES ((square . \"vierkant\") (CIRCLE . \"cirkel\")))
   => (:SHAPES (SQUARE CIRCLE))"
  (list (first ontology-attribute) (mapcar #'car (cadr ontology-attribute))))

(defun get-attribute-value-cons (ontology-attribute)
  "(:SHAPE (SQUARE CIRCLE))
    => ((:SHAPE . SQUARE) (:SHAPE . CIRCLE))"
  (loop for value in (cadr ontology-attribute)
        collect (cons (car ontology-attribute) value)))

(defmethod initialize-instance :after ((world syntax-world) &key &allow-other-keys)
  "Creates the objects in the world, based on the given attributes."
  (let* ((vals (mapcar #'get-attribute-value-cons (attributes world)))
         (non-redundant-vals
          (when (get-configuration (experiment world) :redundant-dimensions)
            (loop for dim-vals in vals
                  unless (find (caar dim-vals)
                               (mapcar #'caar (get-configuration (experiment world) :redundant-dimensions)))
                  collect dim-vals)))
         (objects-attributes (apply #'combinations (or non-redundant-vals
                                                       vals)))
         (redundant-objects-attributes
          (when (get-configuration (experiment world) :redundant-dimensions)
            (loop for object-attributes in objects-attributes
                  collect (append object-attributes
                                  (loop with redundant-dims = (mapcar #'car (get-configuration (experiment world) :redundant-dimensions))
                                        for (dim . value) in object-attributes
                                        for redundant-dim = (loop for dims in redundant-dims
                                                                  when (eql dim (cdr dims))
                                                                  return dims)
                                        for redundant-dim-values = (when redundant-dim
                                                                     (second
                                                                      (find redundant-dim
                                                                            (get-configuration (experiment world) :redundant-dimensions) :key #'first
                                                                            :test #'eql)))
                                        when redundant-dim-values
                                        collect (cons (car redundant-dim)
                                                      (car (find value redundant-dim-values :key #'cdr))))))))
         (objects (loop for object-attributes in (or redundant-objects-attributes
                                                     objects-attributes)
                        for counter from 1 to (length objects-attributes)
                        collect 
                        (make-instance 'syntax-object
                                       :id (make-symbol (string-append "OBJECT-" (write-to-string counter)))
                                       :attributes object-attributes))))
    (setf (objects world) objects)))

(defun perceive-new-scene (world &key (min 1) max)
  "Returns a scene for the current world, consisting of min to max objects." 
  (let* ((max (if max (min max (length (objects world))) (length (objects world))))
         (possible-nr-of-objects (loop for n from min to max collect n))
         (nr-of-objects (random-elt possible-nr-of-objects)))
    (make-instance 'syntax-scene
                   :experiment (experiment world)
                   :objects (get-n-unique-objects (objects world) nr-of-objects))))

(defun select-topic (scene &key (min 1) max)
  "Selects a random subset from a list-of-objects, length min to max."
  (let* ((list-of-objects (objects scene))
         (max (if max (min max (length list-of-objects)) (length list-of-objects)))
         (possible-nr-of-objects (loop for n from min to max collect n))
         (nr-of-objects (random-elt possible-nr-of-objects)))
    (make-instance 'syntax-topic
                   :objects (get-n-unique-objects list-of-objects nr-of-objects))))

(defun get-n-unique-objects (list n)
  "Returns n unique objects from list."
  (unless (<= n (length list))
    (error "Cannot select ~a elements form a list of length ~a." n (length list)))
  (random-elts list n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Discriminating Objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Based on topic and scene only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun discriminate-objects (list-of-objects larger-list-of-objects)
  "For each object in list-of-objects, this function returns a minimal set of discriminative feature-values."
  (mapcar #'(lambda (object)
              (discriminate-object object (remove object larger-list-of-objects))) list-of-objects))

(defun discriminate-object (object list-of-objects)
  "Returns the minimal amount of (attr . val) conses that
   discriminates object from the objects in list-of-objects.
   Make sure the object is not in list-of-objects, otherwise
   the functions will logically return nil."
  (loop for nr-of-attr-needed from 1 to (length (attributes object))
          do
          (let ((attr-combinations (shuffle (combinations-of-length (attributes object) nr-of-attr-needed))))
            (loop for attr-combination in attr-combinations
                  when (discriminative-combination? attr-combination (mapcar #'attributes list-of-objects))
                  do (return-from discriminate-object attr-combination)))))

(defun discriminative-combination? (list-of-attributes list-of-object-attributes)
  "Returns t if the attribute combination in list-of-attributes does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object-attributes in list-of-object-attributes
          while unique
          do
          (let ((attribute-booleans (loop for attribute in list-of-attributes
                                          collect  (equal (cdr attribute)
                                                          (cdr (find (car attribute) object-attributes :key #'car :test #'equal))))))
            (when (apply #'always attribute-booleans)
              (setf unique nil))))
    unique))

(defun get-non-discriminatory-features-for-objects (list-of-object-instances list-of-discriminatory-features-for-objects)
  "Return a list (similar to the
list-of-discriminatory-features-for-objects that contains the NON
discriminatory features for the objects in the
list-of-object-instances."
  (loop for object in list-of-object-instances
        for discriminatory-features-for-object = (loop for feature-list in list-of-discriminatory-features-for-objects
                                                       for d-features-overlapping = (loop for feature in feature-list
                                                                                          when (find feature
                                                                                                     (attributes object) :test #'eql)
                                                                                          collect feature)
                                                       when (= (length d-features-overlapping) (length feature-list)) ;;FULL OVERLAP
                                                       return feature-list)
        collect (set-difference (attributes object)
                                discriminatory-features-for-object)))

;; Based on topic, scene, and only using given words 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun discriminate-objects-using-words (list-of-objects larger-list-of-objects string-cat-meaning-tuples)
  "For each object in list-of-objects, this function returns a minimal set of discriminative feature-values using
   the words in list-of-strings in their order. It is your responsibility to ensure that this is possible!"
  (let* ((possible-feature-combinations
          (remove nil (sort (all-subsequences string-cat-meaning-tuples) #'utils::list-length<l)))
         (possible-feature-combinations-per-object
          (loop for object in list-of-objects
                collect (discriminate-object-using-words object
                                                         (remove object larger-list-of-objects)
                                                         possible-feature-combinations)))
         (possible-feature-combinations-all-objects (apply #'cartesian-product possible-feature-combinations-per-object)))
    (if (= (length possible-feature-combinations-all-objects) 1)
      (first possible-feature-combinations-all-objects)
      (select-feature-combination-by-utterance possible-feature-combinations-all-objects (mapcar #'car string-cat-meaning-tuples)))))

(defun select-feature-combination-by-utterance (feature-combinations list-of-strings)
  "Returns a feature-combination that is coherent with the word order in list-of-strings."
  (loop for f-c in feature-combinations
        for permutations = (permutations-of-length f-c (length f-c))
        when (find list-of-strings permutations :test #'equalp :key #'(lambda (permutation) (remove-if-not #'stringp (flatten permutation))))
        return f-c))

(defun discriminate-object-using-words (object list-of-objects possible-feature-combinations)
  "Returns all shortest possible-feature-combinations that are discriminative for object in list-of-objects."
  (let ((pfcs-to-return nil))
    (loop for p-f-c in (filter-feature-combinations possible-feature-combinations object)
          do
          (when (and (no-shorter-element p-f-c pfcs-to-return)
                     (discriminative-string-cat-meaning-tuple p-f-c (mapcar #'attributes list-of-objects))
                     (push p-f-c pfcs-to-return))))
    (reverse pfcs-to-return)))

(defun no-shorter-element (element list)
  "Returns true if list contains no shorter element."
  (if (null list)
    t
    (let ((shortest-elt-in-list (reduce #'min (mapcar #'length list))))
      (if (<= (length element) shortest-elt-in-list)
        t
        nil))))

(defun discriminative-string-cat-meaning-tuple (list-of-attributes list-of-object-attributes)
  "Returns t if the attribute combination in list-of-attributes does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object-attributes in list-of-object-attributes
          while unique
          do
          (let ((attribute-booleans (loop for attribute in list-of-attributes
                                          collect  (find (third attribute) object-attributes :test #'equalp :key #'cdr))))
            (when (apply #'always attribute-booleans)
              (setf unique nil))))
    unique))

(defun filter-feature-combinations (possible-feature-combinations object)
  "Returns those feature-combinations that are consistent with object"
    (loop for p-f-c in possible-feature-combinations
          when (all-features-in-object-p p-f-c object)
          collect p-f-c))

(defun all-features-in-object-p (string-cat-meaning-tuples object)
  "Returns true if a string-cat-meaning tuple is consistent with object."
  (let ((all-present t)
        (attributes (attributes object)))
    (loop for tuple in string-cat-meaning-tuples
          when (not (find (third tuple) attributes :key #'cdr))
          do (setf all-present nil))
    all-present))

;;;;;;;;;;;;;;;;
;; Goal Tests ;;
;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :single-interpretation-in-world-formulation)))
  "Checks whether the resulting meaning network has only 1 interpretation in the world."
  (let* ((scene (get-data (blackboard (construction-inventory node)) :scene))
         (utterance (render (car-resulting-cfs (cipn-car node))
                            (get-configuration (construction-inventory node) :render-mode)))
         (fresh-cxn-inventory (copy-fcg-construction-set-without-cxns (original-cxn-set (construction-inventory node)))) 
         (meaning nil)
         (possible-topics nil))
    (assert scene)
    ;; Add applied cxns to the fresh inventory
    (dolist (cxn (applied-constructions node))
      (unless (find-cxn (get-original-cxn cxn) fresh-cxn-inventory) ;; Is this correct? Shouldn't the other grammatical cxns be there for triggering the border problem???
        (add-cxn (get-original-cxn cxn) fresh-cxn-inventory)))
    ;; comprehend
    (setf meaning (with-disabled-monitor-notifications
                    (comprehend utterance :cxn-inventory fresh-cxn-inventory)))
    
    (setf possible-topics (get-possible-topics-for-meaning-in-world meaning scene))
  (if (and (= (length possible-topics) 1)
           (equalp (type-of (first possible-topics)) 'syntax-topic))
    t
    (progn
      (set-data (goal-test-data node) :number-of-hypotheses-formulation (length possible-topics))
      nil))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :not-more-than-two-unit-structures)))
    (> 3 (second (multiple-value-list (fcg::connected-syntactic-structure (left-pole-structure (car-resulting-cfs (cipn-car node))))))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :single-interpretation-in-world-comprehension)))
  "Checks whether the resulting meaning network has only 1 interpretation in the world."
  (let* ((scene (get-data (blackboard (construction-inventory node)) :scene))
         (meaning (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (possible-topics (get-possible-topics-for-meaning-in-world meaning scene)))
    (assert scene)
    (if (and (= (length possible-topics) 1)
             (equal (type-of (first possible-topics)) 'syntax-topic))
      t
      (progn
        (set-data (goal-test-data node) :number-of-hypotheses-comprehension (length possible-topics))
        nil))))
    
(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :backtrack-over-grammatical-cxns-only)))
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (unless (priority node)
    (error "The priority of the new node is NIL. You used priority
mode ~a. Please check why it did not calculate a priority score." (get-configuration cip :priority-mode)))
  (if (lex-cxn-in-path-of-sibling-p node)
    (queue cip)
    (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>))))

(defun lexicon (cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        when (eql (attr-val cxn :cxn-type) 'lexical-cxn)
        collect cxn))

(defun markers (cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        when (attr-val cxn :marker)
        collect cxn))

(defun lex-cxn-in-path-of-sibling-p (node)
  (when (and (parent node)
             (siblings node)
             (string= (attr-val (fcg::last-applied-construction node) :cxn-type) 'lexical-cxn))
    (let ((cxns-in-sibling-paths
           (loop for sibling in (siblings node)
                 for sibling-cxn = (fcg::last-applied-construction sibling)
                 append (remove sibling-cxn
                                (remove nil
                                        (traverse-depth-first sibling :collect-fn #'(lambda (node)
                                                                              (when (fully-expanded? node)
                                                                                (fcg::last-applied-construction node)))))
                                :key #'name :test #'equalp))))
      (find (name (fcg::last-applied-construction node)) cxns-in-sibling-paths :key #'name :test #'equalp))))

;;;;;;;;;;;;;;;;;;
;; Cxn-supplier ;;
;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-with-label-nr-of-categories-and-score (cxn-supplier-with-ordered-labels)
  ())

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-nr-of-categories-and-score)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-label-nr-of-categories-and-score
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-label-nr-of-categories-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-by-nr-of-categories-and-score node (car labels)))))))

(defun all-constructions-of-label-by-nr-of-categories-and-score (node label)
  "Returns all constructions that of label 'label', order by number of categories and score."
  (let ((cxns-of-label (copy-object
                        (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                              for cxn-label = (attr-val cxn :label)
                              when (equalp (symbol-name label) (symbol-name cxn-label))
                              collect cxn))))
    (cond ((and (equalp (symbol-name label) "CXN")
                (equal (direction (cip node)) '->))
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((nr-of-cats-cxn1 (length (attr-val cxn1 :categories)))
                                       (nr-of-cats-cxn2 (length (attr-val cxn2 :categories)))
                                       (score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (cond ((> nr-of-cats-cxn1 nr-of-cats-cxn2) t)
                                         ((and (= nr-of-cats-cxn1 nr-of-cats-cxn2) (> score-cxn1 score-cxn2)) t)
                                         (t nil))))))
          ((and (equalp (symbol-name label) "CXN")
                (equal (direction (cip node)) '<-))
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (when (> score-cxn1 score-cxn2) t)))))
          (t cxns-of-label))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-label-nr-of-categories-and-score) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-by-nr-of-categories-and-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems, Diagnostics and Fixes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Classes
;;;;;;;;;;;

(defclass multiple-hypotheses-comprehension (problem)
  ())

(defclass multiple-hypotheses-formulation (problem)
  ())

(defclass diagnose-multiple-hypotheses-comprehension (diagnostic)
  ((trigger :initform 'multiple-hypotheses-comprehension)))

(defclass diagnose-multiple-hypotheses-formulation (diagnostic)
  ((trigger :initform 'multiple-hypotheses-formulation)))

(defclass syntax-fix (fix)
  ())

;; Methods
;;;;;;;;;;;

(defmethod diagnose ((diagnostic diagnose-multiple-hypotheses-formulation) (agent syntax-agent)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated strings"
  (make-instance 'multiple-hypotheses-formulation))

(defmethod diagnose ((diagnostic diagnose-multiple-hypotheses-comprehension) (agent syntax-agent)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated strings"
  (make-instance 'multiple-hypotheses-comprehension))

(defmethod handle-fix ((fix syntax-fix) (repair repair) (problem problem) (agent syntax-agent) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (call-next-method)
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (let* ((node (get-data (blackboard agent) :problematic-cipn))
        (initial-cfs (initial-cfs (cip node))))
      (set-data fix :fixed-cars
                (fcg-apply (processing-cxn-inventory (restart-data fix)) initial-cfs (direction (cip node))))
      ;;(assert (find 'fcg::succeeded (statuses (get-data fix :fixed-cars))))
      (unless (find 'fcg::succeeded (statuses (get-data fix :fixed-cars)))
        (format t "Learning failed in ~a." (if (equal (direction (cip node)) '->) "formulation" "comprehension")))
      ))
