(in-package :naming-game)

(defclass naming-game-interaction (interaction)
  ((utterance :accessor utterance :initarg :utterance :type string :initform nil)
   (pointed-object :accessor pointed-object :initarg :pointed-object  :type symbol :initform nil)))

(defun get-random-elem (list)
  "Gets an element from a list."
  (let* ((size (length list))
         (index (random size)))
    (nth index list)))

(defmethod align ((agent naming-game-agent)(interaction interaction))
  "agent adapts lexicon scores based on communicative success interaction"
  (let ((inc-delta (cdr (assoc :li-incf (configurations agent))))
        (dec-delta (cdr (assoc :li-decf (configurations agent))))
        (communicative-success (communicative-success interaction))
        (alignment (case (cdr (assoc :alignment-strategy (configurations (experiment agent))))
                     (:no-aligment nil)
                     (:lateral-inhibition t))))
    (when alignment
      (cond (communicative-success
             (when (>= (score (applied-voc agent)) 0.5) (print "what does it do?"))
             (increase-score (applied-voc agent) inc-delta 1.0)
             (when (>= (score (applied-voc agent)) 0.5) (print "what does it do?"))
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
  (let ((configuration (cdr (assoc :who-aligns (configurations (experiment interaction)))))
        (speaker (first (interacting-agents interaction)))
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
                                :score (cdr (assoc :initial-score (configurations (experiment agent))))
                                :meaning (topic agent)
                                :form (make-word (experiment agent)))))
  (add-voc agent new-voc)
  new-voc))

(defmethod parse ((agent agent)(interaction interaction))
  "agent tries to parse an utterance and searches its lexicon for voc-items that have the utterance as its form"
  (loop with parsed-voc = nil
        for voc-item in (lexicon agent)
        do (if (string= (form voc-item) (utterance interaction))
             (setf parsed-voc voc-item))
        when parsed-voc
          return parsed-voc))

(defmethod adopt ((agent agent)(interaction interaction))
  "agent adopts a new word and adds it to its own vocabulary"
  (let ((new-voc (make-instance 'voc-item
                                :score (cdr (assoc :initial-score (configurations (experiment agent))))
                                :meaning (topic agent)
                                :form (utterance interaction))))
    (add-voc agent new-voc)
    new-voc))

(defun determine-success (speaker pointed-object)
  "speaker determines whether hearer pointed to right object"
  (cond
   ((null pointed-object)
    nil)
   ((eql pointed-object (topic speaker))
    t)))

(defmethod interact ((experiment experiment) (interaction naming-game-interaction) &key)
  (let* ((interacting-agents (interacting-agents interaction))
         (speaker (first interacting-agents))
         (hearer (second interacting-agents)))
    (setf (topic speaker) (get-random-elem (world experiment)))
    (setf (applied-voc speaker) (produce speaker))
    (unless (applied-voc speaker)
      (setf (applied-voc speaker) (invent speaker)))
    (setf (utterance interaction) (form (applied-voc speaker)))
    (setf (applied-voc hearer) (parse hearer interaction))
    (when (applied-voc hearer)
      (setf (pointed-object interaction) (meaning (applied-voc hearer))))
    (setf (communicative-success speaker) (determine-success speaker (pointed-object interaction)))
    (setf (communicative-success hearer) (communicative-success speaker))
    (setf (topic hearer) (topic speaker))
    (when (and (NOT (communicative-success speaker)) (NOT (applied-voc hearer)))
      (setf (applied-voc hearer) (adopt hearer interaction)))
    (perform-alignment interaction)
    ))
   
  
