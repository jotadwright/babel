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
      (if (null (applied-voc agent)) (print agent))
      (cond (communicative-success
             (increase-score (applied-voc agent) inc-delta 1.0)
             (loop for form-competitor in (get-form-competitors (applied-voc agent) (lexicon agent))
                   do (decrease-score form-competitor dec-delta 0.0)
                   (if (<= (score form-competitor) 0.0)
                       (setf (lexicon agent)(remove form-competitor (lexicon agent) :test #'is-equal))
                       )))
            ((NOT communicative-success)
             (decrease-score (applied-voc agent) dec-delta 0.0)
             (if (<= (score (applied-voc agent)) 0.0)
                 (delete (applied-voc agent) (lexicon agent) :test #'is-equal)))))))

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

(defmethod add-voc ((agent agent) (voc-item voc-item))
  "adds voc-item to the lexicon of agent"
  (push voc-item (lexicon agent)))

(defmethod invent ((agent agent))
  "agent invents a new word and adds it to its lexicon"
  (let ((new-voc (make-instance 'voc-item
                                :score 0.5
                                :meaning (topic agent)
                                :form (make-word (experiment agent)))))
  (add-voc agent new-voc)
  new-voc))

(defmethod parse ((agent agent)(interaction interaction))
  "agent tries to parse an utterance and searches its lexicon for voc-items that have the utterance as its form"
  (loop with parsed-voc = nil
        for voc-item in (lexicon agent)
        do (if (string= (form voc-item) (utterance agent))
             (setf parsed-voc voc-item))
        when parsed-voc
          return parsed-voc))

(defmethod adopt ((agent agent)(interaction interaction))
  "agent adopts a new word and adds it to its own vocabulary"
  (let ((new-voc (make-instance 'voc-item
                                :score 0.5
                                :meaning (topic agent)
                                :form (utterance agent))))
    (add-voc agent new-voc)
    new-voc))

(defun determine-success (speaker pointed-object)
  "speaker determines whether hearer pointed to right object"
  (cond
   ((null pointed-object)
    nil)
   ((eql pointed-object (topic speaker))
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
    (setf (applied-voc speaker) (produce speaker))
    (unless (applied-voc speaker)
      (setf (applied-voc speaker) (invent speaker)))
    (setf (utterance speaker) (form (applied-voc speaker)))
    (setf (utterance hearer) (utterance speaker))
    (when monitor (notify conceptualisation-finished speaker))
    (setf (applied-voc hearer) (parse hearer interaction))
    (when monitor (notify parsing-finished hearer))
    (when (applied-voc hearer)
      (setf (pointed-object hearer) (meaning (applied-voc hearer)))
      (setf (pointed-object speaker) (pointed-object hearer)))
    (when monitor (notify interpretation-finished hearer))
    (setf (communicated-successfully speaker) (determine-success speaker (pointed-object speaker)))
    (setf (communicated-successfully hearer) (communicated-successfully speaker))
    (setf (topic hearer) (topic speaker))
    (unless (applied-voc hearer)
      (setf (applied-voc hearer) (adopt hearer interaction))
      (when monitor (notify adoptation-finished hearer)))
    (perform-alignment interaction)
    (when monitor (notify align-finished))
    ))
   
  
