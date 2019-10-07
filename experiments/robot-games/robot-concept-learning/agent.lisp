(in-package :robot-concept-learning)

;; -------------
;; + MWM agent +
;; -------------
(defclass mwm-agent (agent)
  ((grammar
    :documentation "The agent's grammar"
    :type fcg-construction-inventory :accessor grammar
    :initform (make-agent-grammar))
   (context
    :documentation "The current context (continuous values)"
    :accessor context :initform nil)
   (topic
    :documentation "The current topic"
    :accessor topic :initform nil)
   (applied-cxn
    :documentation "The applied cxns"
    :accessor applied-cxn :initform nil)
   (parsed-meaning
    :documentation "The meaning obtained after parsing"
    :type list :accessor parsed-meaning :initform nil)
   (cxn-history
    :documentation "Maintaining versions of cxns"
    :type list :accessor cxn-history :initform nil)
   (robot
    :documentation "a pointer to the robot in which the agent is loaded"
    :accessor robot :initarg :robot))
  (:documentation "The agent class"))

;; ---------------------------
;; + Agent utility functions +
;; ---------------------------
(defmethod speakerp ((agent mwm-agent))
  (eql (discourse-role agent) 'speaker))

(defmethod hearerp ((agent mwm-agent))
  (eql (discourse-role agent) 'hearer))

(defun make-embodied-agent (experiment)
  (make-instance 'mwm-agent :id 'learner
                 :experiment experiment
                 :robot (make-robot :type 'nao :ip (get-configuration experiment :robot-ip)
                                    :server-port (get-configuration experiment :robot-port))))
  
(defun make-agent-grammar ()
  (let ((grammar-name (make-const "agent-grammar")))
    (eval
     `(def-fcg-constructions ,grammar-name
        :cxn-inventory ,grammar-name
        :feature-types ((args sequence)
                        (form set-of-predicates)
                        (meaning set-of-predicates)
                        (subunits set)
                        (footprints set))))))

;; ---------------
;; + cxn history +
;; ---------------

(defun add-to-cxn-history (agent cxn)
  "Keep the 5 latest versions of each cxn, in json format"
  (let* ((form (attr-val cxn :form))
         (entry (assoc form (cxn-history agent) :test #'string=)))
    (if entry
      (progn
        (when (> (length (rest entry)) 5)
          (setf (rest entry) (butlast (rest entry))))
        (push (cxn->json cxn (get-configuration agent :category-representation))
              (rest entry)))
      (push (cons form (list (cxn->json cxn (get-configuration agent :category-representation))))
            (cxn-history agent)))))

(defun average-over-cxns (list-of-cxns form)
  ;; collect all atttributes that occur in the history of cxns
  (let ((all-attributes (remove-duplicates
                         (loop for json-cxn in list-of-cxns
                               append (loop for meaning-entry in (rest (assoc :meaning json-cxn))
                                            collect (rest (assoc :attribute meaning-entry))))
                         :test #'string=)))
    ;; if an attribute occurs in all 5 of them
    ;; collect the value of this attribute in the latest cxn
    (loop with latest-cxn = (first list-of-cxns)
          for attribute in all-attributes
          when (loop for cxn in list-of-cxns
                     for attributes-with-positive-certainty
                     = (loop for entry in (rest (assoc :meaning cxn))
                             when (> (rest (assoc :certainty entry)) 0.01)
                             collect (rest (assoc :attribute entry)))
                     always (member attribute attributes-with-positive-certainty :test #'string=))
          collect (find attribute (rest (assoc :meaning latest-cxn))
                        :key #'(lambda (m) (rest (assoc :attribute m)))
                        :test #'string=) into meaning
          finally
          (return `((:form . ,form)
                    (:meaning . ,meaning)
                    (:type . ,(rest (assoc :type latest-cxn))))))))
        
(defun average-over-cxn-history (agent)
  (loop for (form . json-cxns) in (cxn-history agent)
        for averaged = (average-over-cxns json-cxns form)
        if (null (rest (assoc :meaning averaged)))
        collect (first json-cxns)
        else
        collect averaged))

;; ---------------------
;; + Receive Utterance +
;; ---------------------

(define-event utterance-received (agent mwm-agent) (utterance string))

(defmethod receive-utterance ((agent mwm-agent))
  "The agent receives an utterance through speech-to-text.
   If no word was recognized, the agent retries"
  (let (utterance)
    (while (null utterance)
      (when (detect-head-touch (robot agent) :middle)
        (let ((detected (first (hear (robot agent) (get-configuration agent :robot-vocabulary)))))
          (when (and (stringp detected) (> (length detected) 0))
            (setf utterance detected)))))
    (notify utterance-received agent utterance)
    utterance))

;; --------------
;; + Parse Word +
;; --------------
(defgeneric parse-word (agent)
  (:documentation "Parse an utterance"))

(define-event parsing-finished (agent mwm-agent))

(defmethod parse-word ((agent mwm-agent))
  "Parse the received utterance"
  (multiple-value-bind (meaning cipn)
      (comprehend (utterance agent) :cxn-inventory (grammar agent))
    (when meaning
      (setf (applied-cxn agent) (get-original-cxn (first (applied-constructions cipn))))
      (setf (parsed-meaning agent) (attr-val (applied-cxn agent) :meaning)))
    (notify parsing-finished agent)
    (parsed-meaning agent)))

;; ------------------
;; + Interpretation +
;; ------------------
(defgeneric interpret (agent)
  (:documentation "Interpret the meaning"))

(define-event interpretation-finished (agent mwm-agent))    
          
(defmethod interpret ((agent mwm-agent))
  (when (parsed-meaning agent)
    (let ((objects-with-similarity
           (loop for object in (objects (context agent))
                 for sim = (weighted-similarity object (parsed-meaning agent))
                 collect (cons object sim))))
      (setf (topic agent)
            (car (the-biggest #'cdr objects-with-similarity)))))
  (notify interpretation-finished agent)
  (topic agent))

;; --------------------
;; + Receive Feedback +
;; --------------------

(define-event ask-for-feedback (agent mwm-agent))
(define-event detection-error (agent mwm-agent) (num-detected number))

(defmethod receive-feedback ((agent mwm-agent))
  (notify ask-for-feedback agent)
  (let ((feedback-set (observe-and-process-world agent)))
    (while (/= (length (objects feedback-set)) 1)
      (notify detection-error agent (length (objects feedback-set)))
      (setf feedback-set (observe-and-process-world agent)))
    feedback-set))
              
;; ------------------
;; + Closest Object +
;; ------------------

(defun closest-object (agent obj)
  (the-smallest #'(lambda (object)
                    (abs (euclidean (get-object-position object)
                                    (get-object-position obj))))
                (objects (context agent))))
      
  

