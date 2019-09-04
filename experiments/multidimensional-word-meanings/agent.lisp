(in-package :mwm)

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
   (symbolic-context
    :documentation "The symbolic clevr context"
    :accessor symbolic-context :initform nil)
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
    :type list :accessor parsed-meaning :initform nil)
   (cxn-history
    :documentation "Maintaining versions of cxns"
    :type list :accessor cxn-history :initform nil))
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
  (let ((tutor (make-instance 'mwm-agent :id 'tutor
                              :experiment experiment)))
    (make-tutor-lexicon tutor)
    tutor))

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
;; + Conceptualisation +
;; ---------------------

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

(defgeneric conceptualise (agent role)
  (:documentation "Conceptualise the topic"))

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
                             all-objects-as-alist :key #'car)))
         (discriminative-set (mapcar #'cdr (discriminate-topic topic-as-alist context-as-alist))))
    (unless (and (get-configuration agent :max-tutor-utterance-length)
                 (length> discriminative-set (get-configuration agent :max-tutor-utterance-length)))
      (setf (discriminative-set agent) discriminative-set))
    (notify conceptualisation-finished agent)
    (discriminative-set agent)))

(defmethod conceptualise ((agent mwm-agent) (role (eql 'learner)))
  "Choose the most discriminating word.
   The word has to have a similarity to the topic that is higher
   than the similarity to any other object. If this is
   the case for multiple words, the one with the biggest difference
   between best-word<->topic and best-word<->best-other-object
   is chosen."
  (loop with best-cxn = nil
        with best-difference = 0
        for cxn in (shuffle (constructions (grammar agent)))
        for cxn-meaning = (attr-val cxn :meaning)
        for topic-similarity = (weighted-similarity (topic agent) cxn-meaning)
        for best-other-similarity = (loop for object in (remove (topic agent) (objects (context agent)))
                                          maximizing (weighted-similarity object cxn-meaning))
        for difference = (- topic-similarity best-other-similarity)
        when (and (> topic-similarity best-other-similarity)
                  (> difference best-difference))
        do (setf best-cxn cxn
                 best-difference difference)
        finally
        (progn (setf (applied-cxns agent)
                     (when best-cxn (list best-cxn)))
          (notify conceptualisation-finished agent)
          (return best-cxn))))

;; --------------
;; + Production +
;; --------------
(defgeneric produce-word (agent role)
  (:documentation "Produce an utterance"))

(define-event production-finished (agent mwm-agent))

(defparameter *synonyms*
  '((cube . ("cube" "block")) (sphere . ("sphere" "ball")) (cylinder . ("cylinder"))
    (gray . ("gray")) (red . ("red")) (blue . ("blue")) (green . ("green"))
    (brown . ("brown")) (purple . ("purple")) (cyan . ("cyan")) (yellow . ("yellow"))
    (left . ("left")) (right . ("right")) (front . ("front")) (behind . ("behind"))
    (small . ("small" "tiny")) (large . ("large" "big"))
    (metal . ("metal" "metallic" "shiny")) (rubber . ("rubber" "matte"))))

(defmethod produce-word ((agent mwm-agent) (role (eql 'tutor)))
  "Simply make strings from the symbols. When lexical variation is
   enabled, the tutor randomly chooses one of the available
   synonyms."
  (setf (utterance agent)
        (if (get-configuration agent :lexical-variation)
          (loop for attr in (discriminative-set agent)
                for synonyms = (rest (assoc attr *synonyms*))
                collect (random-elt synonyms))
          (mapcar (compose #'downcase #'mkstr)
                  (discriminative-set agent))))
  (notify production-finished agent)
  (utterance agent))

(defmethod produce-word ((agent mwm-agent) (role (eql 'learner)))
  (when (applied-cxns agent)
    (setf (utterance agent) (attr-val (first (applied-cxns agent)) :form)))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(defgeneric parse-word (agent role)
  (:documentation "Parse an utterance"))

(define-event parsing-finished (agent mwm-agent))

(defmethod parse-word ((agent mwm-agent) (role (eql 'tutor)))
  t)

(defmethod parse-word ((agent mwm-agent) (role (eql 'learner)))
  "Parse as much words as possible and compute the combined meaning
   using the fuzzy-union operation. Set the applied-cxns and parsed-meaning."
  (multiple-value-bind (meaning cipn)
      (comprehend (utterance agent)
                  :cxn-inventory (grammar agent))
    (when meaning
      (let ((all-meanings
             (loop for cxn in (applied-constructions cipn)
                   collect (attr-val cxn :meaning))))
        (setf (applied-cxns agent) (mapcar #'get-original-cxn
                                           (applied-constructions cipn))
              (parsed-meaning agent) (reduce #'fuzzy-union all-meanings))))
    (notify parsing-finished agent)
    (parsed-meaning agent)))

;; ------------------
;; + Interpretation +
;; ------------------
(defgeneric interpret (agent role)
  (:documentation "Interpret a meaning"))

(define-event interpretation-finished (agent mwm-agent))

(defmethod interpret ((agent mwm-agent) (role (eql 'tutor)))
  ;; how will the symbolic tutor interpret the learner's utterance??
  ;; let's say the learner says 'blue'
  ;; the tutor finds all objects that have 'blue' as an attribute
  ;; but this can be more than one
  ;; how to determine which one to choose as topic? at random??
  (let* ((all-objects-as-alist
          (loop for object in (objects (context agent))
                collect (cons (id object) (object->alist object))))
         (objects-with-utterance
          (loop for (id . object) in all-objects-as-alist
                when (member (utterance agent) (mapcar #'mkstr (mapcar #'cdr object)) :test #'string=)
                collect id)))
    (when objects-with-utterance
      (setf (topic agent)
            (find (random-elt objects-with-utterance)
                  (objects (context agent))
                  :key #'id))))
  (notify interpretation-finished agent)
  (topic agent))
    
          
(defmethod interpret ((agent mwm-agent) (role (eql 'learner)))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (when (parsed-meaning agent)
    (let* ((objects-with-similarity
            (loop for object in (objects (context agent))
                  for sim = (weighted-similarity object (parsed-meaning agent))
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
      (setf (topic agent)
            (unless duplicatesp maybe-topic))))
  (notify interpretation-finished agent)
  (topic agent))
              

;; ---------------------
;; + Determine success +
;; ---------------------
(defun closest-to-topic (speaker hearer-context)
  (let* ((topic (topic speaker))
         (topic-x (typecase topic
                    (clevr-object (x-pos topic))
                    (mwm-object (get-attr-val topic 'x-pos))))
         (topic-y (typecase topic
                    (clevr-object (y-pos topic))
                    (mwm-object (get-attr-val topic 'y-pos)))))
    (the-smallest #'(lambda (object)
                      (abs
                       (euclidean (list topic-x topic-y)
                                  (list (get-attr-val object 'xpos)
                                        (get-attr-val object 'ypos)))))
                  (objects hearer-context))))

(defgeneric determine-success (speaker hearer)
  (:documentation "Determine the success of the interaction"))

(defmethod determine-success ((speaker mwm-agent) (hearer mwm-agent))
  "Compare the IDs of the topics of both agents"
  (if (eql (get-configuration speaker :data-source) :clevr)
    (when (and (topic speaker) (topic hearer))
      (eql (id (topic speaker)) (id (topic hearer))))
    (when (and (topic speaker) (topic hearer))
      (eql (closest-to-topic speaker (context hearer))
           (topic hearer)))))
  

