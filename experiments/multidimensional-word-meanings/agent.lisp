(in-package :mwm)

;; -------------
;; + MWM agent +
;; -------------
(defclass mwm-agent (agent)
  ((grammar
    :documentation "The agent's grammar"
    :type fcg-construction-inventory :accessor grammar
    :initform (make-agent-grammar))
   ;(context
   ; :documentation "The current context (continuous values)"
   ; :accessor context :initform nil)
   ;(symbolic-context
   ; :documentation "The symbolic clevr context"
   ; :accessor symbolic-context :initform nil)
   ;(topic
   ; :documentation "The current topic"
   ; :accessor topic :initform nil)
   ;(applied-cxns
   ; :documentation "The applied cxns"
   ; :type list :accessor applied-cxns :initform nil)
   ;(discriminative-set
   ; :documentation "The discriminative set for the topic"
   ; :type list :accessor discriminative-set :initform nil)
   ;(parsed-meaning
   ; :documentation "The meaning obtained after parsing"
   ; :type list :accessor parsed-meaning :initform nil)
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
          (loop for object in (objects (get-data agent 'clevr-context))
                collect (cons (id object) (object->alist object))))
         (topic (get-data agent 'clevr-topic))
         (topic-as-alist
          (cdr (find (id topic) all-objects-as-alist :key #'car)))
         (context-as-alist
          (mapcar #'cdr
                  (remove-if #'(lambda (id) (eql id (id topic)))
                             all-objects-as-alist :key #'car)))
         (discriminative-set (mapcar #'cdr (discriminate-topic topic-as-alist context-as-alist))))
    (when (and (get-configuration agent :max-tutor-utterance-length)
               (<= (length discriminative-set) (get-configuration agent :max-tutor-utterance-length)))
      (set-data agent 'clevr-conceptualisation discriminative-set))
    (notify conceptualisation-finished agent)
    discriminative-set))

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

#|(defmethod conceptualise ((agent mwm-agent) (role (eql 'learner)))
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
  (find-data agent 'applied-cxns))|#
          
          
(defmethod conceptualise ((agent mwm-agent) (role (eql 'learner)))
  "The learner conceptualises the topic"
  ;; for the moment, there is no incentive for the learner to take the minimal discriminative utterance
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
          (loop for attr in (get-data agent 'clevr-conceptualisation)
                for synonyms = (rest (assoc attr *synonyms*))
                collect (random-elt synonyms))
          (mapcar (compose #'downcase #'mkstr)
                  (get-data agent 'clevr-conceptualisation))))
  (notify production-finished agent)
  (utterance agent))

(defmethod produce-word ((agent mwm-agent) (role (eql 'learner)))
  (when (find-data agent 'applied-cxns)
    (setf (utterance agent)
          (loop for cxn in (find-data agent 'applied-cxns)
                collect (attr-val cxn :form))))
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
        (set-data agent 'applied-cxns
                  (mapcar #'get-original-cxn
                          (applied-constructions cipn)))
        (set-data agent 'parsed-meaning
                  (reduce #'fuzzy-union all-meanings)))))
  (notify parsing-finished agent)
  (find-data agent 'parsed-meaning))

;; ------------------
;; + Interpretation +
;; ------------------
(defgeneric interpret (agent role)
  (:documentation "Interpret a meaning"))

(define-event interpretation-finished (agent mwm-agent))

(defun match-utterance-to-objects (objects utterance)
  (let ((all-objects-as-alist
         (loop for object in objects
               collect (cons (id object) (object->alist object)))))
    (loop for (id . object) in all-objects-as-alist
          for object-attributes = (mapcar (compose #'downcase #'mkstr #'cdr) object)
          when (loop for form in utterance
                     always (member form object-attributes :test #'string=))
          collect (find id objects :key #'id))))

(defun get-spatial-relation (utterance)
  (loop for relation in '("left" "right" "front" "behind")
        thereis (find relation utterance :test #'string=)))

(defun apply-relative-relation (objects relation)
  (if (length= objects 1) objects
    (list
     (cond ((string= relation "left") ; take the leftmost one, i.e. take the object with smallest x
            (extremum objects :key #'x-pos :test #'<))
           ((string= relation "right")
            (extremum objects :key #'x-pos :test #'>))
           ((string= relation "front")
            (extremum objects :key #'y-pos :test #'>))
           ((string= relation "behind")
            (extremum objects :key #'y-pos :test #'<))))))

(defmethod interpret ((agent mwm-agent) (role (eql 'tutor)))
  ;; if the learner says 'blue', the tutor will find
  ;; all objects that are indeed blue. If the tutor finds more
  ;; than one object, interpretation fails.
  ;; this should also work for multi-word utterances.
  (let ((objects-with-utterance
         (match-utterance-to-objects (objects (get-data agent 'clevr-context)) (utterance agent))))    
    (when (and objects-with-utterance
               (length= objects-with-utterance 1))
      (set-data agent 'interpreted-topic (first objects-with-utterance))))
  (notify interpretation-finished agent)
  (find-data agent 'interpreted-topic))
    
          
(defmethod interpret ((agent mwm-agent) (role (eql 'learner)))
  "The agent computes the weighted similarity between the parsed-meaning
   and each of the objects in the context. The topic is the
   object for which this value is maximized."
  (when (find-data agent 'parsed-meaning)
    (let* ((objects-with-similarity
            (loop with parsed-meaning = (find-data agent 'parsed-meaning)
                  for object in (objects (get-data agent 'context))
                  for sim = (weighted-similarity object parsed-meaning)
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
  (if (and (eql (id speaker) 'tutor) (eql (id hearer) 'learner))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'topic))
              (id (get-data hearer 'interpreted-topic))))
    (and (find-data hearer 'interpreted-topic)
         (eql (id (get-data speaker 'clevr-topic))
              (id (get-data hearer 'interpreted-topic))))))
  

