(in-package :naming-game)


(defclass naming-game-experiment (experiment)
  ())

(defmethod initialize-instance :after ((experiment naming-game-experiment) &key)
  (setf (agents experiment) (make-agents experiment)
        (world experiment) (make-world experiment)))


(defun get-random-elem (list)
  "Gets an element from a list."
  (let* ((size (length list))
         (index (random size)))
    (nth index list)))

(defmethod align ((agent naming-game-agent)(interaction interaction))
  "agent adapts lexicon scores based on communicative success interaction"
  (let ((inc-delta (get-configuration agent :li-incf))
        (dec-delta (get-configuration agent :li-decf) )
        (communicative-success (communicated-successfully agent))
        (alignment (case (get-configuration agent :alignment-strategy)
                     (:no-aligment nil)
                     (:lateral-inhibition t))))
    (when alignment
      (cond (communicative-success
             (increase-score (applied-cxn agent) inc-delta 1.0)
             (loop for form-competitor in (get-form-competitors agent)
                   do (decrease-score form-competitor dec-delta 0.0)
                   (if (<= (:score (attributes form-competitor)) 0.0)
                     (delete-cxn form-competitor (lexicon agent)))))
            ((NOT communicative-success)
             (decrease-score (applied-cxn agent) dec-delta 0.0)
             (if (<= (:score (attributes form-competitor)) 0.0)
               (delete-cxn form-competitor (lexicon agent))))))))

(defun perform-alignment (interaction)
  "decides which agents should perform alignment using configurations of interaction"
  (let* ((speaker (first (interacting-agents interaction)))
         (configuration (get-configuration speaker :who-aligns))
         (hearer (second (interacting-agents interaction))))
    (case configuration
      (:both (progn (align hearer interaction) (align speaker interaction)))
      (:hearer (align hearer interaction))
      (:speaker (align speaker interaction)))))

(defmethod highest-score-voc (considered-voc)
  "chooses voc-item in considered-voc with the highest score"
    (loop with highest-voc = nil
          for voc-item in considered-voc
          do (cond
               ((NOT highest-voc) (setf highest-voc voc-item))
               (( > (score voc-item) (score highest-voc))
                (setf highest-voc voc-item)))
          finally (return highest-voc)))

(defmethod add-naming-game-cxn (agent (form string) (meaning list) &key (score 0.5))
  (let ((cxn-name (make-symbol (string-append form "-cxn")))
        (unit-name (make-var (string-append form "-unit"))))
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            (
                             <-
                             (,unit-name
                              (HASH meaning ,meaning)
                              --
                              (HASH form ((string ,unit-name ,new-form)))))
                            :cxn-inventory ',(lexicon agent)
                            :attributes (:score ,score
                                         :form ,form)))
      (declare (ignorable cxn-set))
      cxn)))

(defmethod invent ((agent agent))
  "agent invents a new construction and adds it to its lexicon"
  (let* ((new-form (make-word))
         (new-cxn (add-naming-game-cxn agent new-form (list (topic agent)))))
    (multiple-value-bind (utterance applied-cxn)
        (naming-game-produce agent)
      (values utterance applied-cxn))))
  
(defmethod naming-game-adopt ((agent naming-game-agent))
  "agent adopts a new word and adds it to its own vocabulary"
  (let ((adopted-cxn (add-naming-game-cxn agent (utterance agent) (list (topic agent)))))
    adopted-cxn))


(defun determine-success (speaker pointed-object)
  "speaker determines whether hearer pointed to right object"
  (cond
   ((null pointed-object)
    nil)
   ((eql (first pointed-object) (topic speaker))
    t)))

(defmethod run-interaction ((experiment experiment)
                            &key &allow-other-keys)
  "runs an interaction by increasing the interaction number"
  (let* ((interaction (make-instance
                      'interaction
                      :experiment experiment
                      :interaction-number (if (interactions experiment)
                                            (+ 1 (interaction-number
                                                  (car (interactions experiment))))
                                            1)))
        (monitor
         (if (get-configuration experiment :record-every-x-interactions)
           (when 
               (or 
                (= (mod (interaction-number interaction) (get-configuration experiment :record-every-x-interactions)) 0) 
                (= (interaction-number interaction) 1))
             t)
           t)))
    (push interaction (interactions experiment))
    (determine-interacting-agents experiment interaction
                                  (get-configuration experiment
                                                     :determine-interacting-agents-mode))
    (when monitor (notify interaction-started experiment interaction (interaction-number interaction)))
    (interact experiment interaction)
    (setf (communicated-successfully interaction)
          (loop for agent in (interacting-agents interaction)
                always (communicated-successfully agent)))
    (when monitor (notify interaction-finished experiment interaction (interaction-number interaction)))
    (values interaction experiment)))

(defmethod interact ((experiment experiment) (interaction interaction) &key)
  (let* ((interacting-agents (interacting-agents interaction))
         (speaker (first interacting-agents))
         (hearer (second interacting-agents))
         (monitor
         (if (get-configuration experiment :record-every-x-interactions)
           (when 
               (or 
                (= (mod (interaction-number interaction) (get-configuration experiment :record-every-x-interactions)) 0) 
                (= (interaction-number interaction) 1))
             t)
           t)))
    (setf (topic speaker) (get-random-elem (world experiment)))
    (multiple-value-bind (utterance applied-cxn)
        (naming-game-produce speaker)
      (setf (applied-cxn speaker) applied-cxn)
      (setf (utterance speaker) utterance))
    (unless (applied-cxn speaker)
      (multiple-value-bind (utterance applied-cxn)
          (invent speaker)
        (setf (utterance speaker) utterance)
        (setf (applied-cxn speaker) applied-cxn)))
    (setf (utterance hearer) (utterance speaker))
    (when monitor (notify conceptualisation-finished speaker))
    (setf (pointed-object hearer) (parse (utterance hearer) (lexicon hearer)))
    (when monitor (notify parsing-finished hearer))
    (when (pointed-object hearer)
      (setf (pointed-object speaker) (pointed-object hearer)))
    (when monitor (notify interpretation-finished hearer))
    (setf (communicated-successfully speaker) (determine-success speaker (pointed-object speaker)))
    (setf (communicated-successfully hearer) (communicated-successfully speaker))
    (setf (topic hearer) (topic speaker))
    (unless (pointed-object hearer)
      (setf (applied-cxn hearer)(naming-game-adopt (hearer interaction)))
      (when monitor (notify adoptation-finished hearer)))
    (perform-alignment interaction)
    (when monitor (notify align-finished))
    ))
   
  
