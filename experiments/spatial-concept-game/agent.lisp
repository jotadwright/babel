(in-package :spatial-concepts)

;; -------------
;; + SPATIAL agent +
;; -------------
(defclass spatial-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon"
    :type list :accessor lexicon :initform nil)
   (concept-history
    :documentation "Maintaining versions of concepts"
    :type list :accessor concept-history :initform nil)
   (pointed-object
    :documentation "The object the tutor points to in production"
    :type spatial-object :accessor pointed-object :initform nil))
  (:documentation "The agent class"))

;; ---------------------------
;; + Agent utility functions +
;; ---------------------------
(defmethod speakerp ((agent spatial-agent))
  (eql (discourse-role agent) 'speaker))

(defmethod hearerp ((agent spatial-agent))
  (eql (discourse-role agent) 'hearer))

(defmethod learnerp ((agent spatial-agent))
  (eql (id agent) 'learner))

(defmethod tutorp ((agent spatial-agent))
  (eql (id agent) 'tutor))

(defun make-tutor-agent (experiment)
  (make-instance 'spatial-agent :id 'tutor
                 :experiment experiment))

(defun make-learner-agent (experiment)
  (make-instance 'spatial-agent :id 'learner
                 :experiment experiment))


;;;;
;;;; concept history
;;;;
(defun add-to-concept-history (agent concept)
  "Keep the 5 latest versions of each concept"
  (let ((entry (assoc (form concept) (concept-history agent) :test #'string=))
        (copy (copy-object concept)))
    (if entry
      (progn
        (when (> (length (rest entry)) 5)
          (setf (rest entry) (butlast (rest entry))))
        (push copy (rest entry)))
      (push (cons (form concept) (list copy))
            (concept-history agent)))))

(defun average-over-concepts (list-of-concepts)
  (let ((all-attributes
         (remove-duplicates
          (loop for concept in list-of-concepts
                append (loop for prototype in (meaning concept)
                             collect (attribute prototype))))))
    ;; if an attribute occurs in all 5 concepts in the concept history
    ;; return it
    (loop with latest-concept = (first list-of-concepts)
          for attribute in all-attributes
          when (loop for concept in list-of-concepts
                     for attributes-with-positive-certainty
                     = (loop for prototype in (meaning concept)
                             when (> (certainty prototype) 0.01)
                             collect (attribute prototype))
                     always (member attribute attributes-with-positive-certainty))
          collect (find attribute (meaning latest-concept)
                        :key #'attribute) into meaning
          finally (return meaning))))
        
(defun average-over-concept-history (agent)
  (loop for (form . concepts) in (concept-history agent)
        for averaged-meaning = (average-over-concepts concepts)
        collect (make-instance 'concept :form form :meaning averaged-meaning)))

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(define-event conceptualisation-finished (agent spatial-agent))

(defgeneric conceptualise (agent)
  (:documentation "run conceptualisation"))

(defmethod conceptualise ((agent spatial-agent))
  (loop while t
        for succes = (run-conceptualisation agent)
        if succes return succes
        else do (sample-topic (experiment agent))))

(defmethod run-conceptualisation ((agent spatial-agent))
  (let* ((all-objects (objects (get-data agent 'tutor-context)))
        (topic (get-data agent 'tutor-topic))
        (context-objects (remove topic all-objects)))
        (discriminate-topic context-objects topic agent)
        (notify conceptualisation-finished agent)
        ;; returns the spatial relation or nil
        (find-data agent 'tutor-conceptualisation)))


(defun discriminate-topic (context-objects topic agent)
  (loop for object in context-objects
        for object-relations = (object->alist object)
        for discriminative-relationship
        = (loop for relationship in object-relations
                if (and (member (id topic) (cdr relationship))
                        (= (length (cdr relationship)) 1))
                return (car relationship))
        when discriminative-relationship
        do (progn
             (set-data agent 'tutor-conceptualisation discriminative-relationship)
             (setf (pointed-object agent) (clevr->simulated object))
             (return))))
  
  
;; --------------
;; + Production +
;; --------------
(define-event production-finished (agent spatial-agent))

(defgeneric produce-word (agent)
  (:documentation "Produce an utterance"))

(defmethod produce-word ((agent spatial-agent))
  (setf (utterance agent)
        (downcase
         (mkstr
          (get-data agent 'tutor-conceptualisation))))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(define-event parsing-finished (agent spatial-agent))

(defgeneric parse-word (agent)
  (:documentation "Parse an utterance"))

(defmethod parse-word ((agent spatial-agent))
  (let ((concept (find (utterance agent) (lexicon agent) :key #'form :test #'string=)))
    (when concept
      (set-data agent 'applied-concept concept)))
  (notify parsing-finished agent)
  (find-data agent 'applied-concept))

;; ------------------
;; + Interpretation +
;; ------------------
(define-event interpretation-finished (agent spatial-agent))

(defgeneric interpret (agent)
  (:documentation "Interpret a meaning"))

(defmethod interpret ((agent spatial-agent))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (when (find-data agent 'applied-concept)
    (let* ((objects-with-similarity
            (loop with concept = (find-data agent 'applied-concept)
                  for object in (objects (get-data agent 'context))
                  for sim = (weighted-similarity object concept (pointed-object agent))
                  collect (cons object sim)))
           ;; if two objects have exactly the same
           ;; maximum similarity, interpretation fails
           (highest-pair
            (the-biggest #'cdr objects-with-similarity))
           (maybe-topic (car highest-pair))
           (duplicatesp (> (count (cdr highest-pair)
                                  objects-with-similarity
                                  :key #'cdr :test #'=)
                           1)))
      (when duplicatesp
        (format t "stop here"))
      (set-data agent 'interpreted-topic
                (unless duplicatesp maybe-topic))))
  (notify interpretation-finished agent)
  (find-data agent 'interpreted-topic))
              
;; ---------------------
;; + Determine success +
;; ---------------------

(defgeneric determine-success (speaker hearer)
  (:documentation "Determine the success of the interaction"))

(defmethod determine-success ((speaker spatial-agent) (hearer spatial-agent))
  (if (and (eql (id speaker) 'tutor)
           (eql (id hearer) 'learner))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'topic))
              (id (get-data hearer 'interpreted-topic))))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'tutor-topic))
              (id (get-data hearer 'interpreted-topic))))))
  

