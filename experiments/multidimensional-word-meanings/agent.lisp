(in-package :mwm)

;; -------------
;; + MWM agent +
;; -------------
(defclass mwm-agent (agent)
  ((grammar
    :documentation "The agent's grammar"
    :type fcg-construction-inventory :accessor grammar
    :initform (make-agent-grammar))
   (ontology
    :documentation "The agent's ontology"
    :type blackboard :accessor ontology
    :initform (make-blackboard))
   (context
    :documentation "The current context"
    :accessor context :initform nil)
   (topic
    :documentation "The current topic"
    :accessor topic :initform nil)
   (applied-cxns
    :documentation "The applied cxns"
    :type list :accessor applied-cxns :initform nil)
   (discriminative-set
    :documentation "The discriminative set for the topic"
    :type list :accessor discriminative-set :initform nil)
   (parsed-meaning
    :documentation "The meaning obtained after parsing"
    :type list :accessor parsed-meaning :initform nil))
  (:documentation "The agent class"))

;; ---------------------------
;; + Agent utility functions +
;; ---------------------------
(defmethod speakerp ((agent mwm-agent))
  (eql (discourse-role agent) 'speaker))

(defmethod hearerp ((agent mwm-agent))
  (eql (discourse-role agent) 'hearer))

(defmethod learnerp ((agent mwm-agent))
  (eql (id agent) 'learner))

(defmethod tutorp ((agent mwm-agent))
  (eql (id agent) 'tutor))

(defun make-tutor-agent (experiment)
  (make-instance 'mwm-agent :id 'tutor
                 :experiment experiment))

(defun make-learner-agent (experiment)
  (make-instance 'mwm-agent :id 'learner
                 :experiment experiment))

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

;; ------------------
;; + Discrimination +
;; ------------------
#|
(defun categorise (object categories)
  "Determine the closest category to the object
   from the list of categories."
  (multiple-value-bind (closest-category distance)
      (the-smallest #'(lambda (category)
                        (distance object category))
                    categories)
    (cons closest-category (abs distance))))

(defmethod closest-categories ((agent mwm-agent) (object mwm-object))
  "Create a fuzzy-set representation of the object, using the closest
   categories weighted by the distance between the category and the object"
  (loop for attr in (get-configuration agent :attributes)
        for categories = (find-data (ontology agent) attr)
        when categories
        collect (categorise object categories)))   
        
(defmethod discriminating-categories ((agent mwm-agent) (object mwm-object)
                                      list-of-objects)
  "Create a fuzzy-set representation of the object, using categories
   that are discriminating for that object weighted by the distance
   between the category and the object"
  (loop for attr in (get-configuration agent :attributes)
        for categories = (find-data (ontology agent) attr)
        for (closest-category . distance) = (when categories
                                              (categorise object categories))
        for (closest-other-category . other-distance)
        = (when categories
            (loop for other-object in list-of-objects
                  collect (categorise other-object categories) into lst
                  finally
                  (return (extremum (find-all closest-category lst :key #'car)
                                    :key #'cdr :test #'<))))
        when (and closest-category
                  closest-other-category
                  (<= distance other-distance))
        collect (cons closest-category distance)))
|#

;; ---------------------
;; + Conceptualisation +
;; ---------------------
(defgeneric conceptualise (agent role)
  (:documentation "Conceptualise the topic"))

(defun discriminate-topic (topic list-of-objects)
  "Returns the minimal amount of (attr . val) conses that
   discriminates object from the objects in list-of-objects.
   Make sure the object is not in list-of-objects, otherwise
   the functions will logically return nil."
  (loop for nr-of-attr-needed from 1 to (length topic)
        do (let ((attr-combinations (shuffle (combinations-of-length topic nr-of-attr-needed))))
            (loop for attr-combination in attr-combinations
                  when (discriminative-combination? attr-combination list-of-objects)
                  do (return-from discriminate-topic attr-combination)))))

(defun discriminative-combination? (list-of-attributes list-of-object-attributes)
  "Returns t if the attribute combination in list-of-attributes
   does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object-attributes in list-of-object-attributes
          while unique
          when (loop for attribute in list-of-attributes
                     always (eql (rest attribute)
                                 (rest (assoc (car attribute) object-attributes))))
          do (setf unique nil))
    unique))

(define-event conceptualisation-finished (agent mwm-agent))

;;; Tutor conceptualisation
(defmethod conceptualise ((agent mwm-agent) (role (eql 'tutor)))
  "The tutor uses a symbolic representation of the context and
   computes the minimal discriminative set of attributes"
  (let* ((all-objects-as-alist
          (loop for object in (objects (context agent))
                collect (cons (id object) (object->alist object))))
         (topic-as-alist
          (cdr (find (id (topic agent)) all-objects-as-alist :key #'car)))
         (context-as-alist
          (mapcar #'cdr
                  (remove-if #'(lambda (id) (eql id (id (topic agent))))
                             all-objects-as-alist :key #'car))))
    (setf (discriminative-set agent)
          (mapcar #'cdr (discriminate-topic topic-as-alist context-as-alist)))
    (notify conceptualisation-finished agent)
    (discriminative-set agent)))

;; --------------
;; + Production +
;; --------------
(defgeneric produce-word (agent role)
  (:documentation "Produce an utterance"))

(define-event production-finished (agent mwm-agent))

;;; Tutor production
(defmethod produce-word ((agent mwm-agent) (role (eql 'tutor)))
  "Simply make strings from the symbols"
  (setf (utterance agent)
        (mapcar (compose #'downcase #'mkstr)
                (discriminative-set agent)))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(defgeneric parse-word (agent role)
  (:documentation "Parse an utterance"))

(define-event parsing-finished (agent mwm-agent))

;;; Learner parsing
(defmethod parse-word ((agent mwm-agent) (role (eql 'learner)))
  "Parse as much words as possible and compute the combined meaning
   using the fuzzy-union operation. Set the applied-cxns and parsed-meaning."
  (multiple-value-bind (meaning cipn)
      (comprehend (utterance agent)
                  :cxn-inventory (grammar agent))
    (declare (ignorable meaning))
    (when meaning
      (let ((all-meanings
             (loop for cxn in (applied-constructions cipn)
                   collect (attr-val cxn :meaning))))
        (setf (applied-cxns agent) (applied-constructions cipn)
              (parsed-meaning agent) (reduce #'fuzzy-union all-meanings))))
    (notify parsing-finished agent)
    (parsed-meaning agent)))

;; ------------------
;; + Interpretation +
;; ------------------
(defgeneric interpret (agent role)
  (:documentation "Interpret a meaning"))

(define-event interpretation-finished (agent mwm-agent))

;;; Learner interpretation
(defmethod interpret ((agent mwm-agent) (role (eql 'learner)))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (when (parsed-meaning agent)
    (let ((objects-with-similarity
           (loop for object in (objects (context agent))
                 collect (cons object (weighted-similarity object (parsed-meaning agent))))))
      (setf (topic agent)
            (car (the-biggest #'cdr objects-with-similarity)))))
  (notify interpretation-finished agent)
  (topic agent))
              

;; ---------------------
;; + Determine success +
;; ---------------------
(defgeneric determine-success (speaker hearer)
  (:documentation "Determine the success of the interaction"))

(defmethod determine-success ((speaker mwm-agent) (hearer mwm-agent))
  "Compare the IDs of the topics of both agents"
  (when (and (topic speaker)
             (topic hearer))
    (eql (id (topic speaker))
         (id (topic hearer)))))
  

