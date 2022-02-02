(in-package :mwm)

;; -------------
;; + MWM agent +
;; -------------
(defclass mwm-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon"
    :type list :accessor lexicon :initform nil)
   (concept-history
    :documentation "Maintaining versions of concepts"
    :type list :accessor concept-history :initform nil))
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
(define-event conceptualisation-finished (agent mwm-agent))

(defgeneric conceptualise (agent)
  (:documentation "run conceptualisation"))

(defmethod conceptualise (agent)
  (case (id agent)
    (tutor (tutor-conceptualise agent))
    (learner (learner-conceptualise agent))))


;;;; Tutor conceptualisation
(defmethod tutor-conceptualise ((agent mwm-agent))
  (loop while t
        for success = (run-tutor-conceptualisation agent)
        if success return success
        else do (sample-topic (experiment agent))))

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

(defun discriminate-topic (topic list-of-objects)
  "Returns the minimal amount of (attr . val) conses that
   discriminates object from the objects in list-of-objects.
   Make sure the object is not in list-of-objects, otherwise
   the functions will logically return nil."
  (loop for nr-of-attr-needed from 1 to (length topic)
        for attr-combinations = (shuffle (combinations-of-length topic nr-of-attr-needed))
        do (loop for attr-combination in attr-combinations
                 when (discriminative-combination? attr-combination list-of-objects)
                 do (return-from discriminate-topic attr-combination))))

(defmethod run-tutor-conceptualisation ((agent mwm-agent))
  "The tutor uses a symbolic representation of the context and
   computes the minimal discriminative set of attributes"
  (let* ((all-objects-features
          (loop for object in (objects (get-data agent 'tutor-context))
                collect (cons (id object) (object->alist object))))
         (topic (get-data agent 'tutor-topic))
         (topic-features
          (cdr (find (id topic) all-objects-features :key #'car)))
         (context-features
          (mapcar #'cdr
                  (remove-if #'(lambda (id) (eql id (id topic)))
                             all-objects-features :key #'car)))
         (discriminative-features
          (mapcar #'cdr (discriminate-topic topic-features context-features))))
    ;; fixed to 1 discriminative feature
    (when (length= discriminative-features 1)
      (set-data agent 'tutor-conceptualisation (first discriminative-features)))
    (notify conceptualisation-finished agent)
    ;; returns the feature or nil
    (find-data agent 'tutor-conceptualisation)))


;;;; Learner conceptualisation
(defmethod learner-conceptualise ((agent mwm-agent))
  "In some cases, the tutor cannot even discriminate the topic.
   If this is the case, the learner should not even try"
  (let ((tutor (find 'tutor (population (experiment agent)) :key #'id)))
    (loop while t
          for possible-to-conceptualise-symbolically
          = (with-disabled-monitors (run-tutor-conceptualisation tutor))
          if possible-to-conceptualise-symbolically
          return (run-learner-conceptualisation agent)
          else do (before-interaction (experiment agent)))))

(defmethod run-learner-conceptualisation ((agent mwm-agent))
  "The learner conceptualises the topic using a single cxn."
  (when (lexicon agent)
    (let ((topic (get-data agent 'topic))
          (context (objects (get-data agent 'context))))
      (loop with most-discriminating-concept = nil
            with best-similarity = 0
            with biggest-delta = 0
            for concept in (lexicon agent)
            for topic-similarity = (weighted-similarity topic concept)
            for best-other-similarity
            = (loop for object in (remove topic context)
                    maximize (weighted-similarity object concept))
            for delta = (- topic-similarity best-other-similarity)
            when (and (> topic-similarity best-other-similarity)
                      (> delta biggest-delta)
                      (> topic-similarity best-similarity))
            do (setf most-discriminating-concept concept
                     biggest-delta delta
                     best-similarity topic-similarity)
            finally
            (set-data agent 'applied-concept most-discriminating-concept))
      (notify conceptualisation-finished agent))
    (find-data agent 'applied-concept)))

#|
(defparameter *impossible-combinations*
  (append
   (combinations-of-length '("BLUE-CXN" "BROWN-CXN" "CYAN-CXN" "GRAY-CXN"
                             "GREEN-CXN" "PURPLE-CXN" "RED-CXN" "YELLOW-CXN") 2)
   (combinations-of-length '("BEHIND-CXN" "LEFT-CXN" "RIGHT-CXN" "FRONT-CXN") 2)
   (combinations-of-length '("CUBE-CXN" "CYLINDER-CXN" "SPHERE-CXN") 2)
   (combinations-of-length '("METAL-CXN" "RUBBER-CXN") 2)
   (combinations-of-length '("LARGE-CXN" "SMALL-CXN") 2)))

(defun valid-combination-p (cxns)
  (let ((cxn-names (mapcar (compose #'upcase #'mkstr #'name) cxns)))
    (loop for (name-a name-b) in *impossible-combinations*
          never (and (find name-a cxn-names :test #'string=)
                     (find name-b cxn-names :test #'string=)))))

(defun get-discriminating-cxn-for-object (agent object cxns meanings)
  (loop with best-cxn = nil
        with best-similarity = 0
        with largest-difference = 0
        for cxn in cxns
        for meaning in meanings
        for object-similarity = (weighted-similarity object meaning)
        for best-other-similarity
        = (when (> object-similarity 0)
            (loop for other in (remove object (objects (get-data agent 'context)))
                  maximize (weighted-similarity other meaning)))
        for diff = (when best-other-similarity
                     (- object-similarity best-other-similarity))
        when (and object-similarity best-other-similarity
                  (> object-similarity best-other-similarity)
                  (> diff largest-difference)
                  (> object-similarity best-similarity))
        do (setf best-cxn cxn
                 best-similarity object-similarity
                 largest-difference diff)
        finally (return best-cxn)))

(defun get-best-cxn-for-others (agent cxns meanings)
  (loop for object in (remove (get-data agent 'topic)
                              (objects (get-data agent 'context)))
        collect (loop with best-cxn = nil
                      with best-similarity = 0
                      for cxn in cxns
                      for meaning in meanings
                      for object-similarity = (weighted-similarity object meaning)
                      when (> object-similarity best-similarity)
                      do (setf best-cxn cxn
                               best-similarity object-similarity)
                      finally (return best-cxn))))

(defmethod conceptualise ((agent mwm-agent) (role (eql 'learner)))
  (when (constructions (grammar agent))
    (loop for i from 1 to (get-configuration agent :max-tutor-utterance-length)
          for cxns = (if (= i 1) (constructions (grammar agent))
                       (remove-if-not #'valid-combination-p
                                      (combinations-of-length
                                       (constructions (grammar agent)) i)))
          for meanings = (loop for elem in cxns
                               if (listp elem)
                               collect (reduce #'fuzzy-union
                                               (mapcar #'(lambda (cxn) (attr-val cxn :meaning))
                                                       elem))
                               else collect (attr-val elem :meaning))
          for topic-cxn
          = (get-discriminating-cxn-for-object agent (get-data agent 'topic) cxns meanings)
          for other-cxns
          = (when topic-cxn
              (get-best-cxn-for-others agent cxns meanings))
          unless (member topic-cxn other-cxns :test #'equal)
          do (progn (set-data agent 'applied-cxns
                              (if (listp topic-cxn) topic-cxn (list topic-cxn)))
               (return))))
  (notify conceptualisation-finished agent)
  (find-data agent 'applied-cxns))

 (defmethod run-learner-conceptualisation ((agent mwm-agent))
  (when (constructions (grammar agent))
    (let* ((cxns (loop for i from 1 to (get-configuration agent :max-tutor-utterance-length)
                       append (if (= i 1) (constructions (grammar agent))
                                (remove-if-not #'valid-combination-p
                                               (combinations-of-length (constructions (grammar agent)) i)))))
           (meanings (loop for cxn in cxns
                           if (listp cxn)
                           collect (reduce #'fuzzy-union
                                           (mapcar #'(lambda (cxn)
                                                       (attr-val cxn :meaning))
                                                   cxn))
                           else collect (attr-val cxn :meaning)))
           (topic (get-data agent 'topic))
           (context (objects (get-data agent 'context))))
      (when meanings
        (loop with best-cxn = nil
              with best-similarity = 0
              with largest-difference = 0
              for cxn in cxns
              for meaning in meanings
              for topic-similarity = (weighted-similarity topic meaning)
              for best-other-similarity
              = (when (> topic-similarity 0)
                  (loop for object in (remove topic context)
                        maximize (weighted-similarity object meaning)))
              for diff = (when best-other-similarity
                           (- topic-similarity best-other-similarity))
              when (and topic-similarity best-other-similarity
                        (> topic-similarity best-other-similarity)
                        (> diff largest-difference)
                        (> topic-similarity best-similarity))
              do (setf best-cxn cxn
                       best-similarity topic-similarity
                       largest-difference diff)
              finally (set-data agent 'applied-cxns
                                (if (listp best-cxn) best-cxn
                                  (list best-cxn)))))))
  (notify conceptualisation-finished agent)
  (find-data agent 'applied-cxns))
 |#
         
                                 

;; --------------
;; + Production +
;; --------------
(define-event production-finished (agent mwm-agent))

(defgeneric produce-word (agent)
  (:documentation "Produce an utterance"))

(defmethod produce-word ((agent mwm-agent))
  (case (id agent)
    (tutor (tutor-produce-word agent))
    (learner (learner-produce-word agent))))

(defmethod tutor-produce-word ((agent mwm-agent))
  "Simply make strings from the symbols. When lexical variation is
   enabled, the tutor randomly chooses one of the available
   synonyms."
  (setf (utterance agent)
        (downcase
         (mkstr
          (get-data agent 'tutor-conceptualisation))))
  (notify production-finished agent)
  (utterance agent))

(defmethod learner-produce-word ((agent mwm-agent))
  (when (find-data agent 'applied-concept)
    (setf (utterance agent)
          (form (find-data agent 'applied-concept))))
  (notify production-finished agent)
  (utterance agent))

;; -----------
;; + Parsing +
;; -----------
(define-event parsing-finished (agent mwm-agent))

(defgeneric parse-word (agent)
  (:documentation "Parse an utterance"))

(defmethod parse-word ((agent mwm-agent))
  (case (id agent)
    (tutor (tutor-parse-word agent))
    (learner (learner-parse-word agent))))

(defmethod tutor-parse-word ((agent mwm-agent))
  t)

(defmethod learner-parse-word ((agent mwm-agent))
  "Parse as much words as possible and compute the combined meaning
   using the fuzzy-union operation. Set the applied-cxns and parsed-meaning.
   This is overkill since only single words are being used."
  (let ((concept (find (utterance agent) (lexicon agent) :key #'form :test #'string=)))
    (when concept
      (set-data agent 'applied-concept concept)))
  (notify parsing-finished agent)
  (find-data agent 'applied-concept))

;; ------------------
;; + Interpretation +
;; ------------------
(define-event interpretation-finished (agent mwm-agent))

(defgeneric interpret (agent)
  (:documentation "Interpret a meaning"))

(defmethod interpret ((agent mwm-agent))
  (case (id agent)
    (tutor (tutor-interpret agent))
    (learner (learner-interpret agent))))

(defun match-utterance-to-objects (objects utterance)
  (let ((all-objects-as-alist
         (loop for object in objects
               collect (cons (id object) (object->alist object)))))
    (loop for (id . object) in all-objects-as-alist
          for object-attributes = (mapcar (compose #'downcase #'mkstr #'cdr) object)
          when (loop for form in utterance
                     always (member form object-attributes :test #'string=))
          collect (find id objects :key #'id))))

(defmethod tutor-interpret ((agent mwm-agent))
  ;; if the learner says 'blue', the tutor will find
  ;; all objects that are indeed blue. If the tutor finds more
  ;; than one object, interpretation fails.
  ;; this should also work for multi-word utterances.
  (let ((objects-with-utterance
         (match-utterance-to-objects (objects (get-data agent 'clevr-context))
                                     (utterance agent))))   
    (when (and objects-with-utterance
               (length= objects-with-utterance 1))
      (set-data agent 'interpreted-topic (first objects-with-utterance))))
  (notify interpretation-finished agent)
  (find-data agent 'interpreted-topic))
    
          
(defmethod learner-interpret ((agent mwm-agent))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (when (find-data agent 'applied-concept)
    (let* ((objects-with-similarity
            (loop with concept = (find-data agent 'applied-concept)
                  for object in (objects (get-data agent 'context))
                  for sim = (weighted-similarity object concept)
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
      (set-data agent 'interpreted-topic
                (unless duplicatesp maybe-topic))))
  (notify interpretation-finished agent)
  (find-data agent 'interpreted-topic))
              

;; ---------------------
;; + Determine success +
;; ---------------------

(defgeneric determine-success (speaker hearer)
  (:documentation "Determine the success of the interaction"))

(defmethod determine-success ((speaker mwm-agent) (hearer mwm-agent))
  (if (and (eql (id speaker) 'tutor)
           (eql (id hearer) 'learner))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'topic))
              (id (get-data hearer 'interpreted-topic))))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'tutor-topic))
              (id (get-data hearer 'interpreted-topic))))))
  

