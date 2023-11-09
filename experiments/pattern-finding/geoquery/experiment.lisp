(in-package :pf-for-sql)

(defclass pf-for-sql-experiment (experiment)
  ((corpus :initarg :corpus :initform nil 
           :accessor corpus :type list
           :documentation "A list of question-answer (the answer is predicate network here) pairs."))
  (:documentation "A grammar learning experiment regarding geoquery for sql."))

(defclass pf-for-sql-agent (agent)
  ((lexicon
    :documentation "The lexicon of the agent"
    :initarg :lexicon
    :accessor lexicon
    :initform *fcg-constructions*
    :type construction-inventory)))

(defmethod make-agents ((experiment pf-for-sql-experiment))
  "A method that creates two agents in the experiment : a tutor and a learner."
  (let ((agents (loop for i from 1 to 2
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                        collect (make-instance 'pf-for-sql-agent
                                               :id agent-id
                                               :experiment 'experiment))))
    (setf (population experiment) agents)
    (setf (discourse-role (first (population experiment))) 'tutor-agent)
    (setf (discourse-role (second (population experiment))) 'learner-agent)))

(defun load-corpus (corpus)
  "A function to load the jsonl file : returns a list of utterance-meaning pairs."
  (let ((file-data '()))
    (with-open-file (stream corpus :direction :input :external-format :utf-8 :element-type :default)
      (loop for line = (read-line stream nil nil)
            while line
            do (let*
                   ((processed-line (remove-last-character line))
                    (data (com.inuoe.jzon:parse processed-line))
                    (utterance (gethash "utterance" data)))
                 (with-input-from-string (meaning (gethash "meaning" data))
                   (push-end (list `(:utterance ,utterance) `( :meaning ,meaning)) file-data)))))
    file-data))

(defmethod initialize-instance :after ((experiment pf-for-sql-experiment) (corpus-path string) &key)
  "Create the population and load the corpus."
  (set-configuration experiment :current-stage 0)
  (setf (agents experiment) (make-agents experiment))
  (setf (corpus experiment) (load-corpus corpus-path)))

(defmethod interact ((experiment pf-for-sql-experiment) (interaction interaction) &key)
  "the different steps of an interaction between the given agents"
  ;1) initializes instances
  (let* ((tutor (first interacting-agents))
         (learner (second interacting-agents))
         (utterance))
    ;(activate-monitors experiment interaction)
  ;2) The tutor speaks an utterance
    (setf (utterance tutor) (first (corpus experiment)))
    (setf utterance (assoc ':utterance (utterance tutor)))
  ;3) the hearer either proposes a meaning or fails to understand anything
    (setf (utterance hearer) (comprehend utterance :cxn-inventory (lexicon agent))))
  ;4) if he doesn't know any word for it it creates one
    (unless (applied-cxn speaker)
      (multiple-value-bind (utterance applied-cxn)
          (invent speaker)
        (setf (utterance speaker) utterance)
        (setf (applied-cxn speaker) applied-cxn)))
  ;5) in any case, the hearer hears a word a tries to make sense out of it
    (setf (utterance hearer) (utterance speaker))
    (notify conceptualisation-finished speaker)
    (multiple-value-bind (meaning solution cip)
        (comprehend (utterance hearer) :cxn-inventory (lexicon hearer))
      (setf (pointed-object hearer) (first meaning))
      (setf (applied-cxn hearer) (first (applied-constructions solution))))
    (notify parsing-finished hearer)
  ;6) the hearer tells the speaker what he understood
    (when (pointed-object hearer)
      (setf (pointed-object speaker) (pointed-object hearer)))
    (notify interpretation-finished hearer)
 ;7) if the speaker and the hearer referred to the same object, there is communicative success
 ; if not, there is not communicative success
    (setf (communicated-successfully speaker) (determine-success speaker (pointed-object speaker)))
    (setf (communicated-successfully hearer) (communicated-successfully speaker))
    (setf (topic hearer) (topic speaker))
    (unless (pointed-object hearer)
      (setf (applied-cxn hearer)(naming-game-adopt (hearer interaction) (utterance hearer))) 
      (notify adoptation-finished hearer))
 ;8 the agents perform the alignment according to the above function
    (perform-alignment interaction)
    (notify align-finished)
    ))