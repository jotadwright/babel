(in-package :cl-user)

;----------------;
; Configurations ;
;----------------;

; You can change the configurations of the experiment here:

(defparameter *experiment-configurations*
  '((:population-size . 10)
    (:word-structure . :restricted)
    (:world-size . 10)
    (:scene-size . 5)
    (:alignment-strategy . :lateral-inhibition)
    (:who-aligns . :both)
    (:initial-score . 0.5)
    (:li-incf . 0.1)
    (:li-decf . 0.1)))

;------------------;
; Generating words ;
;------------------;


(defun make-word (experiment)
  "Creates a word of nr-of-syllables where a syllable is a consonant plus a vowel"
  (let ((vowels '("a" "e" "i" "o" "u"))
        (consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
        (word ""))
    (loop for i from 1 to 3
          do (setq word (concatenate 'string word (nth (random (length consonants)) consonants)))
          do (setq word (concatenate 'string word (nth (random (length vowels)) vowels))))
    (push word (all-words experiment))
    word))

;----------------;
; voc-item class ;
;----------------;

; 'voc-item' class for each lexical item in an agent's vocabulary:
(defclass voc-item ()
  ((form :documentation "the form that is produced to refer to the object" :type string :accessor form :initarg :form)
   (meaning :documentation "the object that is refered to by the form" :type symbol :accessor meaning :initarg :meaning)
   (score :documentation "the score that is associated to the word" :type float :accessor score :initarg :score)))


(defmethod is-equal ((item-1 voc-item)(item-2 voc-item))
  "Checks if item-1 is equal to item-2"
  (and (eql (form item-1) (form item-2))(eql (meaning item-1) (meaning item-2))))

(defmethod increase-score ((voc-item voc-item) (delta float) (upper-bound float))
  "Increases score of voc-item with delta and cuts it off at upper-bound"
  (incf (score voc-item) delta)
  (if (> (score voc-item) upper-bound)
      (setf (score voc-item) upper-bound)))

(defmethod decrease-score ((voc-item voc-item) (delta float) (lower-bound float))
  "Decreases score of voc-item with delta and cuts it off at lower-bound"
  (decf (score voc-item) delta)
  (if (<= (score voc-item) lower-bound)
      (setf (score voc-item) lower-bound)))

(defmethod get-form-competitors ((voc-item voc-item) (lexicon list))
  "pick different forms from of lexicon that have the same meaning as voc-item"
  (let ((form-competitors '()))
    (loop for item in lexicon
          do (if (and (eql (meaning item) (meaning voc-item)) (NOT (eql item voc-item)))
                 (push item form-competitors)))
    form-competitors))

;------------------;
; Experiment class ;
;------------------;

; 'experiment' class keeps track of all the useful information in the experiment
(defclass experiment ()
  ((configurations :accessor configurations :initarg :configurations :type cons :initform nil)
   (all-words :accessor all-words :initarg :all-words :type list :initform nil)
   (world :accessor world :initarg :world :type list :initform nil)
   (population :accessor population :initarg :population :type list :initform nil)
   (interactions :accessor interactions :initarg :interactions :type list :initform nil)))

(defmethod make-world ((experiment experiment))
  "Creates the different objects in the world of experiment"
  (let ((objects (loop for i from 1 to (cdr (assoc :world-size (configurations experiment)))
                          collect (read-from-string (format nil "obj-~d" i)))))
    (setf (world experiment) objects)))

(defmethod make-agents ((experiment experiment))
  "Creates the different agents in the population of experiment"
  (let ((agents (loop for i from 1 to (cdr (assoc :population-size (configurations experiment)))
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                      collect (make-instance 'agent
                                             :id agent-id
                                             :experiment experiment
                                             :configurations (configurations experiment)))))
    (setf (population experiment) agents)))

(defmethod initialize-experiment ((experiment experiment))
  "Initializes the experiment by creating the world and the population"
  (format t "~%~%==========================================================")
  (format t "~%~%Initializing experiment with the following configurations:~%")
  (pprint (configurations experiment))
  (make-world experiment)
  (make-agents experiment)
  (format t "~%~%The population of the experiment consists of ~d agents:~%" (length (population experiment)))
  (print-agents experiment)
  (format t "~%~%These agents interact in a world with the following objects:~%")
  (pprint (world experiment))
  (format t "~%~%----------------------------------------------------------~%"))

(defmethod print-agents ((experiment experiment))
  "prints the names of all agents in experiment in a nice and readable way"
  (loop for agent in (population experiment)
        do (print (id agent))))
 
;-------------------;
; interaction class ;
;-------------------;

; 'interaction' class keeps tracks of all the useful information in one interaction of the experiment
(defclass interaction ()
  ((experiment :accessor experiment :initarg :experiment :type experiment :initform nil)
   (interacting-agents :accessor interacting-agents :initarg :interacting-agents :type list :initform nil)
   (utterance :accessor utterance :initarg :utterance :type string :initform nil)
   (pointed-object :accessor pointed-object :initarg :pointed-object  :type symbol :initform nil)
   (communicative-success :accessor communicative-success :initarg :communicative-success :type boolean :initform nil)))

(defun remove-from-list (elements list)
  "Removes elements from list."
  (let ((copy-list list))
    (loop for element in elements
          do (setf copy-list (remove element copy-list)))
    copy-list))


(defmethod combined-voc-size ((interaction interaction))
  "Gets the combined-voc-size of all agents in the experiment"
  (let ((complete-vocab '()))
    (loop for agent in (population (experiment interaction))
          do (loop for voc-item in (lexicon agent)
                   do (if (NOT (member voc-item complete-vocab :test #'is-equal))
                        (push voc-item complete-vocab))))
    (length complete-vocab)))

(defun determine-success (speaker pointed-object)
  "speaker determines whether hearer pointed to right object"
  (cond
   ((null pointed-object)
    nil)
   ((eql pointed-object (topic speaker))
    t)))

(defun perform-alignment (interaction)
  "decides which agents should perform alignment using configurations of interaction"
  (let ((configuration (cdr (assoc :who-aligns (configurations (experiment interaction)))))
        (speaker (first (interacting-agents interaction)))
        (hearer (second (interacting-agents interaction))))
  (case configuration
      (:both (progn (align hearer interaction) (align speaker interaction)))
      (:hearer (align hearer interaction))
      (:speaker (align speaker interaction)))))

(defun run-interaction (experiment)
  "runs one single interaction of experiment"
  (let* ((interaction (make-instance 'interaction :experiment experiment))
        (interacting-agents (choose-elems (population experiment) 2))
        (speaker (clear (first interacting-agents)))
        (hearer (clear (second interacting-agents))))
    (setf (interacting-agents interaction) interacting-agents)
    (setf (topic speaker) (get-random-elem (world experiment)))
    (monitor-beginning interaction speaker hearer)
    (setf (applied-voc speaker) (produce speaker))
    (unless (applied-voc speaker)
      (setf (applied-voc speaker) (invent speaker)))
    (setf (utterance interaction) (form (applied-voc speaker)))
    (setf (applied-voc hearer) (parse hearer interaction))
    (monitor-parse-produce interaction hearer)
    (when (applied-voc hearer)
      (setf (pointed-object interaction) (meaning (applied-voc hearer))))
    (setf (communicative-success interaction) (determine-success speaker (pointed-object interaction)))
    (setf (topic hearer) (topic speaker))
    (monitor-success interaction speaker hearer)
    (when (and (NOT (communicative-success interaction)) (NOT (applied-voc hearer)))
      (setf (applied-voc hearer) (adopt hearer interaction)))
    (perform-alignment interaction)
    (monitor-alignment speaker hearer)
    (push interaction (interactions experiment))
    (monitor-end interaction)))

(defun choose-elems (list n)
  "Chooses n elements from list."
  (loop with chosen-elements = '()
        for i from 1 to n
        for remaining-elements = (remove-from-list chosen-elements list)
        for nr-remaining-elements = (length remaining-elements)
        for chosen-element = (nth (random nr-remaining-elements) remaining-elements)
        do (push chosen-element chosen-elements)
        finally (return chosen-elements)))

(defun get-random-elem (list)
  "Gets an element from a list."
  (let* ((size (length list))
         (index (random size)))
    (nth index list)))

(defun monitor-beginning (interaction speaker hearer)
  "Monitors the beginning of an interaction where speaker and hearer are chosen ; speaker chooses a topic."
  (let ((interaction-number (+ 1 (length (interactions (experiment interaction))))))
    (format t "~%~%This is interaction number ~d." interaction-number)
    (format t "~%~%The two interacting agents are ~a and ~a.:~%" (id speaker)(id hearer))
    (format t "~a is the speaker and ~a is the hearer.~%" (id speaker) (id hearer))
    (format t "~%The speaker chooses ~a as the current topic.~%" (topic speaker))
    ))

(defun monitor-parse-produce (interaction hearer)
  "Monitors how speaker produces an utterance and hearer tries to parse it."
  (format t "~%The speaker produces utterance '~a' to refer to the topic.~%" (utterance interaction))
  (if (applied-voc hearer)
    (format t "~%The hearer parsed the utterance and points to object ~a.~%" (meaning (applied-voc hearer)))))

(defun monitor-success (interaction speaker hearer)
  "Monitors if the interaction was successful or not."
  (if (communicative-success interaction)
    (format t "~%The interaction was successful.")
  (progn (format t "~%The interaction failed.~%")
    (format t "~%The speaker points to ~a.~%" (topic speaker))
    (format t "~%Hearer adopts a new word that has '~a' as its form and ~a as its meaning. The initial score is set to ~d.~%" (utterance interaction) (topic hearer) (cdr (assoc :initial-score (configurations (experiment interaction))))))))

(defun monitor-alignment (speaker hearer)
  "Monitors if there was alignment or not."
   (format t "~%The agents will now perform alignment~%")
   (format t "~%New vocabulary of the speaker:~%")
   (print-vocabulary speaker)
   (format t "~%~%New vocabulary of the hearer:~%")
   (print-vocabulary hearer))

(defun monitor-end (interaction)
  "Monitors the end of the interaction : the lexicons and the lexical-coherence."
  (format t "~%The total amount of words in use by the agents is now ~d~%" (combined-voc-size interaction))
  (format t "~%The the average lexicon size in the population is now ~g~%" (average-lexicon-size interaction))
  (format t "~%Lexical coherence for this example? ~a~%" (lexical-coherence interaction))
  (format t "~%The average number of forms per meaning is now ~g~%" (forms-per-meaning interaction))
  (format t "~%--------------------------------------------------------------------~%"))

(defun average-lexicon-size (interaction)
  "Computes the average lexicon-size of the agents in interaction"
  (let* ((summed-lexicon-size 0)
         (nr-of-agents (length (population (experiment interaction)))))
    (loop for agent in (population (experiment interaction))
          for lexicon-length = (length (lexicon agent))
          do (incf summed-lexicon-size lexicon-length))
    (unless (eql summed-lexicon-size 0)(/ summed-lexicon-size nr-of-agents))))

(defun lexical-coherence (interaction)
  "Determines if speaker and hearer of interaction would choose the same word to refer to topic"
  (let* ((speaker (first (interacting-agents interaction)))
         (hearer (second (interacting-agents interaction)))
         (hearer-voc nil)
         (speaker-voc (applied-voc speaker)))
    (produce hearer)
    (setf hearer-voc (applied-voc hearer))
    (if (is-equal hearer-voc speaker-voc)
      "yes"
      "no")))

(defun forms-per-meaning (interaction)
  "Computes how many forms exist for each object in the experiment"
  (let ((combined-agent-meanings
         (loop for agent in (population (experiment interaction))
               for different-meanings = 0
               for meaning-list = '()
               for all-meanings = (length (lexicon agent))
               do (loop for voc-item in (lexicon agent)
                        do (unless (member (meaning voc-item) meaning-list)
                             (incf different-meanings)))
               collect (if (or (eql 0 all-meanings) (eql 0 different-meanings))
                         0
                         (/ all-meanings different-meanings))))
        (all-experiment-meanings 0))
    (loop for agent-meaning in combined-agent-meanings
          do (incf all-experiment-meanings agent-meaning))
    (/ all-experiment-meanings (length (population (experiment interaction))))))
                 
;-------------;
; agent class ;
;-------------;

;'agent' represents one agent in the population of the experiment and contains all information related to this agent
(defclass agent ()
  ((id :documentation "unique identifier that can be used to point to the agent" :type symbol :accessor id :initarg :id)
   (experiment :documentation "pointer to the experiment the agent belongs to" :type experiment :accessor experiment :initarg :experiment)
   (configurations :documentation "the configurations of the experiment the agent belongs to" :type cons :accessor configurations :initarg :configurations)
   (role :documentation "the role the agent has in the current discourse (speaker or listener)" :type symbol :accessor role :initarg :discourse-role)
   (lexicon :documentation "all the vocabulary-items of the agent" :type list :accessor lexicon :initarg :lexicon :initform nil)
   (context :documentation "the objects in the context" :type list :accessor context :initarg :context)
   (topic :documentation "the topic object that is chosen by the speaker" :type symbol :accessor topic :initarg :topic)
   (applied-voc :documentation "the voc-item that is applied" :type voc-item :accessor applied-voc :initarg :applied-voc :initform nil)
   (utterance :documentation "the utterance that is produced or heard" :type string :accessor utterance :initarg :utterance)
   (success :documentation "indicates whether the interaction was sucessfull" :type boolean :accessor success :initarg :success)))

(defmethod align ((agent agent)(interaction interaction))
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

(defmethod print-vocabulary ((agent agent))
  "prints the lexicon of agent in a nice and readable way"
  (loop with counter = 0
        for voc-item in (lexicon agent)
        do (incf counter)
        collect (format t "Item #~d: form = ~a, meaning = ~a, score = ~a~%" counter (form voc-item)(meaning voc-item)(score voc-item))
        ))

(defmethod clear ((agent agent))
  "clears information about previous interactions from the agent's slots"
  (setf (topic agent) nil)
  (setf (applied-voc agent) nil)
  (setf (utterance agent) nil)
  agent)

(defmethod produce ((agent agent))
  "agent tries to produce a word that refers to the topic object"
  (let ((considered-voc '()) ;list of voc-items (form, meaning, score) that have meaning as the topic
        (chosen-voc nil)
        (lexicon (lexicon agent)))
    (when lexicon
      (loop for voc-item in lexicon
            do (if (eql (meaning voc-item) (topic agent))
                 (push voc-item considered-voc)))
      (cond ((= (length considered-voc) 1) (setf chosen-voc (first considered-voc)))
            ((> (length considered-voc) 1) (setf chosen-voc (highest-score-voc considered-voc)))))
    chosen-voc))


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


;------------------------;
; Running the experiment ;
;------------------------;

(defun run-series (experiment nr-of-interactions)
  "runs nr-of-interaction of experiment"
  (loop for i from 1 to nr-of-interactions
        do (run-interaction experiment))
  (format t "~%~%==========================================================")
  experiment)


;(defparameter *experiment* (make-instance 'experiment :configurations *experiment-configurations*))
;(initialize-experiment *experiment*)
;(run-series *experiment* 1000)
