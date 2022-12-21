(in-package :naming-game)

;-----------------;
;general functions;
;-----------------;

(defclass naming-game-experiment (experiment)
  ())

(defmethod initialize-instance :after ((experiment naming-game-experiment) &key)
  "creates new instances"
  (setf (agents experiment) (make-agents experiment)
        (world experiment) (make-world experiment)))


(defun get-random-elem (list)
  "Gets an element from a list."
  (let* ((size (length list))
         (index (random size)))
    (nth index list)))

(defun clear (agent)
  "sets a variable to nil"
  (setf (pointed-object agent) nil)
  (setf (applied-cxn agent) nil)
  (setf (pointed-object agent) nil))

(defmethod add-naming-game-cxn (agent (form string) (meaning list) &key (score 0.5))
  "agent adds a construction to its construction inventory ; sends back the construction"
  (let ((cxn-name (make-symbol (string-append form "-cxn")))
        (unit-name (make-var (string-append form "-unit")))
        )
    (multiple-value-bind (cxn-set cxn)
        (eval `(def-fcg-cxn ,cxn-name
                            (
                             <-
                             (,unit-name
                              (HASH meaning ,meaning)
                              --
                              (HASH form ((string ,unit-name ,form)))))
                            :cxn-inventory ',(lexicon agent)
                            :attributes (:score ,score
                                         :form ,form
                                         :meaning ,meaning)))
      (declare (ignorable cxn-set))
      cxn)))

(defun activate-monitors (experiment interaction)
  "activates the monitors needed to do the tracing"
  (deactivate-monitor trace-interaction-wi)
  (deactivate-monitor trace-experiment-wi)
  (if (get-configuration experiment :trace-every-x-interactions)
    (when 
        (or 
         (= (mod (interaction-number interaction) (get-configuration experiment :trace-every-x-interactions)) 0) 
         (= (interaction-number interaction) 1))
      (activate-monitor trace-interaction-wi)
      (activate-monitor trace-experiment-wi))
    (activate-monitor trace-interaction-wi))
    (activate-monitor trace-experiment-wi))

;-------------------;
;alignment functions;
;-------------------;

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
             (when (applied-cxn agent)(increase-score (applied-cxn agent) inc-delta 1.0))
             (loop for form-competitor in (get-form-competitors agent)
                   do (decrease-score form-competitor dec-delta 0.0)))
            ((NOT communicative-success)
             (when (applied-cxn agent)(decrease-score (applied-cxn agent) dec-delta 0.0)))))))

(defun perform-alignment (interaction)
  "decides which agents should perform alignment using configurations of interaction"
  (let* ((speaker (first (interacting-agents interaction)))
         (configuration (get-configuration speaker :who-aligns))
         (hearer (second (interacting-agents interaction))))
    (case configuration
      (:both (progn (align hearer interaction) (align speaker interaction)))
      (:hearer (align hearer interaction))
      (:speaker (align speaker interaction)))))


;----------------------;
;word related functions;
;----------------------;

(defmethod invent ((agent agent))
  "agent invents a new construction and adds it to its lexicon"
  (let* ((new-form (make-word))
         (new-cxn (add-naming-game-cxn agent new-form (list (topic agent)))))
    (multiple-value-bind (utterance applied-cxn)
        (naming-game-produce agent)
      (values utterance applied-cxn))))
  
(defmethod naming-game-adopt ((agent naming-game-agent)(cxn-form string)) ;we pass cxn-form as an argument because it is not supposed to be the cxn-form of the same agent (I guess?)
  "agent adopts a new word and adds it to its own vocabulary"
  (let ((adopted-cxn (add-naming-game-cxn agent cxn-form (list (topic agent)))))
    adopted-cxn))

;-------------------------;
;success related functions;
;-------------------------;

(defmethod highest-score-voc (considered-voc)
  "chooses voc-item in considered-voc with the highest score"
    (loop with highest-voc = nil
          for voc-item in considered-voc
          do (cond
               ((NOT highest-voc) (setf highest-voc voc-item))
               (( > (score voc-item) (score highest-voc))
                (setf highest-voc voc-item)))
          finally (return highest-voc)))

(defun determine-success (speaker pointed-object)
  "speaker determines whether hearer pointed to right object"
  (cond
   ((null pointed-object)
    nil)
   ((eql pointed-object (topic speaker))
    t)))


;-----------------------;
;running one interaction;
;-----------------------;


(defmethod interact ((experiment experiment) (interaction interaction) &key)
  "the different steps of an interaction between the given agents"
  ;1) initializes instances
  (let* ((interacting-agents (interacting-agents interaction))
         (speaker (first interacting-agents))
         (hearer (second interacting-agents)))
    (activate-monitors experiment interaction)
  ;2) we choose a topic for the experiment (a random object from our world)
    (setf (topic speaker) (get-random-elem (world experiment)))
  ;3) the speaker tries to produce a word for this topic
    (multiple-value-bind (utterance applied-cxn)
        (naming-game-produce speaker)
      (setf (applied-cxn speaker) applied-cxn)
      (setf (utterance speaker) utterance))
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
   
  
