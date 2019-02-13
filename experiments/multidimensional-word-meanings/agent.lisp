(in-package :mwm)

(defclass mwm-agent (agent)
  ((ontology
    :documentation "The agent's ontology"
    :type blackboard :initform (make-blackboard)
    :accessor ontology)
   (lexicon
    :documentation "The agent's grammar"
    :type list :initform nil
    :accessor lexicon)
   (topic
    :documentation "The topic of the current interaction"
    :type (or null mwm-object) :accessor topic)
   (context
    :documentation "The context of the current interaction"
    :type (or null mwm-object-set) :accessor context)
   (discriminating-categories
    :documentation "Discriminating categories for the topic"
    :type list :initform nil :accessor discriminating-categories)
   (applied-lex
    :documentation "List of applied lex items"
    :type list :initform nil :accessor applied-lex))
  (:documentation "The agent"))

;; The current implementation of the different parts of the interaction script
;; are based on Pieter Wellens' PhD thesis. We have to keep in mind, however,
;; that the categories of the world are finite and known in advance in Pieter's
;; case. In our case, the categories are being shaped at the same time. This leads
;; to the question of when and where to introduce new categories or re-use the
;; closest existing one.

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defun get-channel-categories (agent channel)
  "Get all the known categories on the given channel"
  (find-data (ontology agent) channel))
  
(defmethod categorise-object ((agent mwm-agent) (object mwm-object))
  "For each channel, compute the one closest to the object"
  (let ((channels (get-configuration agent :channels)))
    (loop for channel in channels
          for channel-categories = (get-channel-categories agent channel)
          for (best-channel similarity)
          = (multiple-value-list
             (the-biggest #'(lambda (c)
                              (channel-similarity object c))
                          channel-categories))
          collect (list channel best-channel similarity))))

(defun get-best-categorisation-for-context (context-categorisations channel)
  (loop with the-best = nil
        for obj-cat in context-categorisations
        for obj-cat-channel = (find channel (rest obj-cat) :key #'first)
        when (or (null the-best)
                 ;; higher similarity
                 (> (third obj-cat-channel) (third the-best)))
        do (setf the-best obj-cat-channel)
        finally (return the-best)))

(defun valid-categorisation-p (topic-cat best-other-cat strategy)
  (case strategy
    (:nearest
     (if (eql (second topic-cat) (second best-other-cat))
       (> (third topic-cat) (third best-other-cat))
       t))
    (:discrimination
     (not (eql (second topic-cat) (second best-other-cat))))))
        
(defmethod conceptualise ((agent mwm-agent) (topic mwm-object))
  "Find discrete categories for the topic. This depends on the
   conceptualisation strategy. When using :nearest-neighbour,
   other objects can have the same category as the topic, but the topic
   must be closest to it. When using :discrimination, no other object
   is allowed to have the same category as the topic."
  (let* ((strategy (get-configuration agent :conceptualisation-strategy))
         (object-categorisations
          (loop for object in (entities (context agent))
                collect (cons object (categorise-object agent object))))
         (topic-categorisation
          (find topic object-categorisations :key #'car))
         (context-categorisations
          (remove topic object-categorisations :key #'car)))
    (setf (discriminating-categories agent)
          (loop with discriminating-categories = nil
                for channel in (get-configuration agent :channels)
                for topic-cat = (find channel (rest topic-categorisation) :key #'first)
                for best-other-cat = (get-best-categorisation-for-context context-categorisations channel)
                when (valid-categorisation-p topic-cat best-other-cat strategy)
                do (push topic-cat discriminating-categories)
                finally
                (return (loop for (channel category similarity) in discriminating-categories
                              collect (cons category similarity)))))))
  
;; --------------
;; + Production +
;; --------------

(defun utterance-meaning (agent utterance)
  "Construct the meaning of a multi-word utterance
   by taking the fuzzy-union of all meanings of all forms
   in the utterance."
  (let ((lex-items
         (loop for form in utterance
               for lex = (find form (lexicon agent) :key #'form :test #'string=)
               when lex
               collect lex)))
    (values (reduce #'fuzzy-union lex-items :key #'meaning :initial-value '())
            lex-items)))

(defun overlap (meaning object-categories)
  "Compute the overlap between the meaning of a form
   and the result of conceptualisation"
  (float (/ (- (* (fuzzy-cardinality (fuzzy-intersection meaning object-categories))
                  (fuzzy-cardinality (fuzzy-intersection object-categories meaning)))
               (* (fuzzy-cardinality (fuzzy-difference meaning object-categories))
                  (fuzzy-cardinality (fuzzy-difference object-categories meaning))))
            (* (fuzzy-cardinality meaning) (fuzzy-cardinality object-categories)))))

(defun get-best-new-word (lexicon utterance utterance-meaning object)
  (let ((lex-to-consider (remove-if #'(lambda (lex)
                                        (member (form lex) utterance :test #'string=))
                                    lexicon)))
    (loop with best-lex = nil
          with best-overlap = nil
          for lex in lex-to-consider
          for extended-meaning = (fuzzy-union (meaning lex) utterance-meaning)
          for overlap = (overlap extended-meaning object)
          when (or (null best-lex)
                   (> overlap best-overlap))
          do (setf best-lex lex
                   best-overlap overlap)
          finally
          (return best-lex))))
        
(defmethod produce ((agent mwm-agent))
  "Try to express as many attributes of the topic by adding
   new words such that the overlap between the aggregated meaning
   and the topic increases."
  ;; in the lexicon, the meanings are fuzzy sets
  ;; so the discriminating-categories should also be a fuzzy set
  ;; for now, the certainty of the latter are equal to the similarity
  ;; of that category to the topic. This could also change to being 1 everywhere.
  (when (and (discriminating-categories agent)
             (lexicon agent))
    (setf (applied-lex agent)
          (loop with utterance = nil
                with utterance-lexs = nil
                with utterance-meaning = nil
                with utterance-similarity = nil
                with continue = t
                while continue
                for best-new-word = (get-best-new-word (lexicon agent)
                                                       utterance
                                                       utterance-meaning
                                                       (discriminating-categories agent))
                for extended-meaning = (fuzzy-union (meaning best-new-word) utterance-meaning)
                for new-similarity = (overlap extended-meaning (discriminating-categories agent))
                if (and (plusp new-similarity)
                        (or (null utterance-similarity)
                            (> new-similarity utterance-similarity)))
                do (progn (push (form best-new-word) utterance)
                     (push best-new-word utterance-lexs)
                     (setf utterance-meaning extended-meaning
                           utterance-similarity new-similarity))
                else
                do (setf continue nil)
                finally
                (return utterance-lexs)))
    (setf (utterance agent)
          (when (applied-lex agent)
            (mapcar #'form (applied-lex agent)))))
  (utterance agent))
                    
         
;; ---------------
;; + Re-entrance +
;; ---------------

(defmethod re-enter ((agent mwm-agent))
  "Interpret the utterance returned by production. Check if
   it leads to the correct topic."
  (when (utterance agent)
    (eql (topic agent) (interpret agent (utterance agent)))))

;; -------------
;; + Invention +
;; -------------

(defmethod invent ((agent mwm-agent))
  "Invent a new form. Create new categories on the channels
   that were not discriminating. These categories will take the
   value of the topic as initial value. Associate these new
   categories to the newly created form with a low certainty score.
   If the used meaning already closely resembles the topic, don't invent.
   Only a slight shift in meaning will be necessary."
  ;; Now, only new categories are expressed
  ;; What about discriminating categories already there?
  (let* ((utterance-meaning (utterance-meaning agent (utterance agent)))
         (topic-similarity (when utterance-meaning
                             (overlap utterance-meaning (discriminating-categories agent)))))
    (when (or (null utterance-meaning)
              (and utterance-meaning
                   (> (random 1.0) topic-similarity)))
      (let* ((all-channels (get-configuration agent :channels))
             (used-channels (mapcar #'channel (mapcar #'car (discriminating-categories agent))))
             (unused-channels (set-difference all-channels used-channels))
             (new-categories
              (loop for channel in unused-channels
                    for topic-value-on-channel = (access-channel (topic agent) channel)
                    collect (add-category agent channel topic-value-on-channel))))
        ;; after invention, production will be retried
        ;; so, we set these new categories as discriminating categories
        ;; with certainty 1
        (setf (discriminating-categories agent)
              (loop for c in new-categories
                    collect (cons c 1)))
        (add-lex agent (make-new-word) new-categories)))))

;; ------------------
;; + Interpretation +
;; ------------------

(defmethod interpret ((agent mwm-agent) (utterance string))
  "Search for the object in the context that maximizes the overlap
   with the meaning of the utterance (if known)."
  (multiple-value-bind (parsed-utterance-meaning applied-lexs)
      (utterance-meaning agent utterance)
    (when (hearerp agent)
      (setf (applied-lex agent) applied-lexs))
    (loop with best-object = nil
          with best-overlap = nil
          for object in (entities (context agent))
          for object-categorisation = (let ((categorisation (categorise-object agent object)))
                                        (loop for (channel category similarity) in categorisation
                                              collect (cons category similarity)))
          for overlap = (overlap parsed-utterance-meaning object-categorisation)
          when (or (null best-object)
                   (> overlap best-overlap))
          do (setf best-object object
                   best-overlap overlap)
          finally
          (return best-object))))

;; ------------
;; + Adoption +
;; ------------

(defmethod adopt ((agent mwm-agent) (topic mwm-object))
  (let (;; determine the first unkown form
        (new-form (loop for form in (utterance agent)
                        unless (find form (lexicon agent) :key #'form :test #'string=)
                        return form)))
    (when new-form
      (let* (;; determine the unexpressed channels
             (utterance-meaning (utterance-meaning agent (utterance agent)))
             (used-channels (mapcar #'channel (mapcar #'car utterance-meaning)))
             (all-channels (get-configuration agent :channels))
             (unused-channels (set-difference all-channels used-channels))
             ;; categorise all objects on all channels
             (strategy (get-configuration agent :conceptualisation-strategy))
             (object-categorisations
              (loop for object in (entities (context agent))
                    collect (cons object (categorise-object agent object))))
             (topic-categorisation
              (find topic object-categorisations :key #'car))
             (context-categorisations
              (remove topic object-categorisations :key #'car)))
        ;; find discriminating categories for the topic on the unused channels
        ;; if no category is discriminating, create a new one
        (loop with new-meaning = nil
              for channel in unused-channels
              for topic-on-channel = (find channel (rest topic-categorisation) :key #'first)
              for best-other-on-channel = (get-best-categorisation-for-context context-categorisations channel)
              if (valid-categorisation-p topic-on-channel best-other-on-channel strategy)
              do (push (second topic-on-channel) new-meaning)
              else
              do (push (add-category agent channel (access-channel (topic agent) channel)) new-meaning)
              finally
              ;; store these categories as meaning of the first, unknown word
              do (add-lex agent new-form new-meaning))))))

;; ---------------------
;; + Determine Success +
;; ---------------------

(defmethod determine-success ((speaker mwm-agent) (hearer mwm-agent))
  (when (topic hearer)
    (eql (id (topic speaker))
         (id (topic hearer)))))
  
    
    
;; ------------------------------
;; + Tutor Ontology and Lexicon +
;; ------------------------------

;;;; The tutor ontology and lexicon
(defun build-tutor-ontology-and-lexicon (agent)
  "Add categories to the ontology and words to the lexicon"
  (let ((left-category (make-instance 'x-pos-channel :value (scale-channel :x-y-pos 50)))
        (x-middle-category (make-instance 'x-pos-channel :value (scale-channel :x-y-pos 250)))
        (right-category (make-instance 'x-pos-channel :value (scale-channel :x-y-pos 450)))
        (bottom-category (make-instance 'y-pos-channel :value (scale-channel :x-y-pos 50)))
        (y-middle-category (make-instance 'y-pos-channel :value (scale-channel :x-y-pos 250)))
        (top-category (make-instance 'y-pos-channel :value (scale-channel :x-y-pos 450)))
        (small-area-category (make-instance 'area-channel :value (scale-channel :area 2500)))
        (medium-area-category (make-instance 'area-channel :value (scale-channel :area 40000)))
        (large-area-category (make-instance 'area-channel :value (scale-channel :area 122500)))
        (narrow-category (make-instance 'width-channel :value (scale-channel :width-height 50)))
        (medium-width-category (make-instance 'width-channel :value (scale-channel :width-height 200)))
        (wide-category (make-instance 'width-channel :value (scale-channel :width-height 350)))
        (tall-category (make-instance 'height-channel :value (scale-channel :width-height 50)))
        (medium-height-category (make-instance 'height-channel :value (scale-channel :width-height 200)))
        (short-category (make-instance 'height-channel :value (scale-channel :width-height 350)))
        (red-category (make-instance 'mean-color-channel :value (scale-channel :mean-color '(255 0 0))))
        (green-category (make-instance 'mean-color-channel :value (scale-channel :mean-color '(0 255 0))))
        (blue-category (make-instance 'mean-color-channel :value (scale-channel :mean-color '(0 0 255))))
        (shiny-category (make-instance 'stdev-color-channel :value (scale-channel :stdev-color '(1.75 1.75 1.75))))
        (matte-category (make-instance 'stdev-color-channel :value (scale-channel :stdev-color '(0.4 0.4 0.4))))
        (4-sides-category (make-instance 'nr-sides-channel :value (scale-channel :nr-of-sides 4)))
        (3-sides-category (make-instance 'nr-sides-channel :value (scale-channel :nr-of-sides 3)))
        (10-sides-category (make-instance 'nr-sides-channel :value (scale-channel :nr-of-sides 10)))
        (4-corners-category (make-instance 'nr-corners-channel :value (scale-channel :nr-of-corners 4)))
        (3-corners-category (make-instance 'nr-corners-channel :value (scale-channel :nr-of-corners 3)))
        (0-corners-category (make-instance 'nr-corners-channel :value (scale-channel :nr-of-corners 0)))
        (square-ratio-category (make-instance 'wh-ratio-channel :value 1.0))
        (rectangle-ratio-category (make-instance 'wh-ratio-channel :value 0.75)))
    (set-data (ontology agent) :x-pos (list left-category x-middle-category right-category))
    (set-data (ontology agent) :y-pos (list bottom-category y-middle-category top-category))
    (set-data (ontology agent) :area (list small-area-category medium-area-category large-area-category))
    (set-data (ontology agent) :width (list narrow-category medium-width-category wide-category))
    (set-data (ontology agent) :height (list tall-category medium-height-category short-category))
    (set-data (ontology agent) :mean-color (list red-category green-category blue-category))
    (set-data (ontology agent) :stdev-color (list shiny-category matte-category))
    (set-data (ontology agent) :nr-of-sides (list 4-sides-category 3-sides-category 10-sides-category))
    (set-data (ontology agent) :nr-of-corners (list 4-corners-category 3-corners-category 0-corners-category))
    (set-data (ontology agent) :wh-ratio (list square-ratio-category rectangle-ratio-category))
    (add-lex agent "square" (list 4-sides-category 4-corners-category square-ratio-category)
             :initial-certainty 1.0)
    (add-lex agent "triangle" (list 3-sides-category 3-corners-category square-ratio-category)
             :initial-certainty 1.0)
    (add-lex agent "circle" (list 10-sides-category 0-corners-category square-ratio-category)
             :initial-certainty 1.0)
    (add-lex agent "rectangle" (list 4-sides-category 4-corners-category rectangle-ratio-category)
             :initial-certainty 1.0)
    (add-lex agent "red" (list red-category) :initial-certainty 1.0)
    (add-lex agent "green" (list green-category) :initial-certainty 1.0)
    (add-lex agent "blue" (list blue-category) :initial-certainty 1.0)
    (add-lex agent "small" (list small-area-category narrow-category short-category)
             :initial-certainty 1.0)
    (add-lex agent "medium" (list medium-area-category medium-width-category medium-height-category)
             :initial-certainty 1.0)
    (add-lex agent "large" (list large-area-category wide-category tall-category)
             :initial-certainty 1.0)
    (add-lex agent "shiny" (list shiny-category) :initial-certainty 1.0)
    (add-lex agent "matte" (list matte-category) :initial-certainty 1.0)
    (add-lex agent "left" (list left-category) :initial-certainty 1.0)
    (add-lex agent "center" (list x-middle-category) :initial-certainty 1.0)
    (add-lex agent "right" (list right-category) :initial-certainty 1.0)
    (add-lex agent "bottom" (list bottom-category) :initial-certainty 1.0)
    (add-lex agent "middle" (list y-middle-category) :initial-certainty 1.0)
    (add-lex agent "top" (list top-category) :initial-certainty 1.0)
    agent))