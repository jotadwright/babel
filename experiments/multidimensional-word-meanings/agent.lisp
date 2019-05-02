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
  (let ((tutor (make-instance 'mwm-agent :id 'tutor
                              :experiment experiment)))
    (when (eql (get-configuration experiment :tutor-lexicon) :continuous)
      (make-tutor-lexicon tutor))
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
  (case (get-configuration agent :tutor-lexicon)
    (:symbolic (conceptualise-symbolic agent))
    (:continuous (conceptualise-continuous agent))))

(defmethod conceptualise-symbolic ((agent mwm-agent))
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


;;; Tutor conceptualisation with continuous values.

;;; According to Wellens' adaptive strategy:
;;; loop with utterance = nil
;;;      while t
;;;      for best-new-word = (argmax for word in words (overlap (union word utterance) topic))
;;;      for new-similarity = (overlap (union utterance best-new-word) topic)
;;;      if new-similarity > previous-similarity >= 0
;;;      do (setf utterance (union best-new-word utterance))
;;;      else (return utterance)

;;; This immediately allows for multi-word utterances.
;;; To constrain for single word, add it to the while-test.
;;; Should the concept of discrimination be used?
;;; Should the concept of re-entrance be used?

(defun choose-best-word (topic cxns meaning-so-far)
  "Choose the best word to add to the utterance. This function
   simply takes the word with the highest similarity."
  (extremum cxns
            :key #'(lambda (cxn)
                     (let* ((cxn-meaning (attr-val cxn :meaning))
                            (meaning (fuzzy-union cxn-meaning meaning-so-far)))
                       (weighted-similarity topic meaning)))
            :test #'>))

(defun choose-discriminating-word (topic context cxns meaning-so-far)
  (loop with best-cxn = nil
        with best-similarity = 0
        for cxn in cxns
        for cxn-meaning = (attr-val cxn :meaning)
        for meaning = (fuzzy-union cxn-meaning meaning-so-far)
        for topic-similarity = (weighted-similarity topic meaning)
        for best-other-similarity = (loop for object in context
                                          maximizing (weighted-similarity object meaning))
        when (and (> topic-similarity best-other-similarity)
                  (> topic-similarity best-similarity))
        do (setf best-cxn cxn
                 best-similarity topic-similarity)
        finally
        do (return best-cxn)))

(defmethod conceptualise-continuous ((agent mwm-agent))
  (loop with utterance = nil ; list of cxns
        with utterance-meaning = nil ; combined meaning
        with best-similarity = 0
        with continue = t
        ; utterance has a max length
        while (and (length< utterance (get-configuration agent :max-tutor-utterance-length))
                   continue)
        for best-new-word = (choose-best-word (topic agent) (constructions (grammar agent)) utterance-meaning)
        for new-similarity = (let* ((cxn-meaning (attr-val best-new-word :meaning))
                                    (extended-meaning (fuzzy-union cxn-meaning utterance-meaning)))
                               (weighted-similarity (topic agent) extended-meaning))
        if (> new-similarity best-similarity)
        do (progn (push best-new-word utterance)
             (setf utterance-meaning (fuzzy-union (attr-val best-new-word :meaning) utterance-meaning))
             (setf best-similarity new-similarity))
        else
        do (setf continue nil)
        finally
        do (progn (setf (applied-cxns agent) utterance)
             (notify conceptualisation-finished agent)
             (return utterance))))

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


;;; Tutor production
(defmethod produce-word ((agent mwm-agent) (role (eql 'tutor)))
  "Simply make strings from the symbols. When lexical variation is
   enabled, the tutor randomly chooses one of the available
   synonyms."
  (setf (utterance agent)
        (if (eql (get-configuration agent :tutor-lexicon) :symbolic)
          (if (get-configuration agent :lexical-variation)
            (loop for attr in (discriminative-set agent)
                  for synonyms = (rest (assoc attr *synonyms*))
                  collect (random-elt synonyms))
            (mapcar (compose #'downcase #'mkstr)
                    (discriminative-set agent)))
          (mapcar #'(lambda (cxn) (attr-val cxn :form))
                  (applied-cxns agent))))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(defun sample-features (meaning)
  "Sample a subset of the features according to the
   certainty of that feature."
  (loop for (category . certainty) in meaning
        for r = (random 1.0)
        when (< r certainty)
        collect (cons category certainty) into meaning-sample
        finally
        (return meaning-sample)))

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
        (setf (applied-cxns agent) (mapcar #'get-original-cxn
                                           (applied-constructions cipn))
              (parsed-meaning agent) (reduce #'fuzzy-union all-meanings))
        ;; when :feature-selection is set to :sampling; do the sampling here
        ;; and overwrite the parsed-meaning slot of the agent
        (when (eql (get-configuration agent :feature-selection) :sampling)
          (setf (parsed-meaning agent) (sample-features (parsed-meaning agent))))))
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
    (let* ((objects-with-similarity
            (loop for object in (objects (context agent))
                  for sim = (weighted-similarity object (parsed-meaning agent))
                  collect (cons object sim)))
           (highest-pair
            (the-biggest #'cdr objects-with-similarity))
           (maybe-topic (car highest-pair)))
      ;; sanity check
      ;; no guessing if multiple objects have the same 'highest' similarity
      (setf (topic agent)
            (unless (> (count (cdr highest-pair)
                              objects-with-similarity
                              :key #'cdr :test #'=)
                       1)
              maybe-topic))))
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
  

