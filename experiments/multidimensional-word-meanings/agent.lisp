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
    :type (or null mwm-object-set) :accessor context))
  (:documentation "The agent"))

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defun get-channel-categories (agent channel)
  "Get all the known categories on the given channel"
  (find-all channel (get-data (ontology agent) 'channels) :key #'channel))
  
(defun categorise-object ((agent mwm-agent) (object mwm-object))
  "For each channel, compute the one closest to the object"
  (let ((channels (get-configuration agent :channels)))
    (loop for channel in channels
          for channel-categories = (get-channel-categories agent channel)
          for (best-channel similarity)
          = (the-biggest #'(lambda (c)
                             (channel-similarity object c))
                         channel-categories)
          collect (list channel best-channel similarity))))

(defmethod conceptualise ((agent mwm-agent) (topic mwm-object))
  "Find discriminating channels for the topic. Other object can have
   the same category on a given channel, as long as they are not closer
   to the topic"
  (let* ((object-categorisations
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
                for best-other-cat = (loop with the-best = nil
                                           for obj-cat in context-categorisations
                                           for obj-cat-channel = (find channel (rest obj-cat) :key #'first)
                                           if (or (null the-best) (> (third obj-cat-channel) (third the-best)))
                                           do (setf the-best obj-cat-channel)
                                           finally
                                           (return the-best))
                unless (and (eql (second topic-cat) (second best-other-cat))
                            (< (third topic-cat) (third best-other-cat)))
                do (push topic-cat discriminating-categories)
                finally
                (return (loop for (channel category similarity) in discriminating-categories
                              collect (cons category similarity)))))))
  
;; --------------
;; + Production +
;; --------------

(defun overlap (meaning object-categories)
  "Compute the overlap between the meaning of a form
   and the result of conceptualisation"
  (float (/ (- (* (fuzzy-cardinality (fuzzy-intersection meaning object-categories))
                  (fuzzy-cardinality (fuzzy-intersection object-categories meaning)))
               (* (fuzzy-cardinality (fuzzy-difference meaning object-categories))
                  (fuzzy-cardinality (fuzzy-difference object-categories meaning))))
            (* (fuzzy-cardinality meaning) (fuzzy-cardinality object-categories)))))

(defmethod produce ((agent mwm-agent))
  "Find the word that maximizes the overlap between the
   meaning of the word and the conceptualised topic"
  (when (and (discriminating-categories agent)
             (lexicon agent))
    (setf (applied-lex agent)
          (loop with best-lex = nil
                with best-overlap = nil
                for lex in (lexicon agent)
                for overlap = (overlap (meaning lex) (discriminating-categories agent))
                when (or (null best-lex) (> overlap best-overlap))
                do (setf best-lex lex
                         best-overlap overlap)
                finally
                (return best-lex)))
    (setf (utterance agent)
          (when (applied-lex agent)
            (form (applied-lex agent)))))
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
  "Invent a new form. Associate all discriminating categories
   to it. Additionally, create new categories on the channels
   that were not discriminating. These categories will take the
   value of the topic as initial value."
  ;; in Wellens, the meaning in invention consists of only the unexpressed categories of the topic
  (let* ((new-form (make-new-word))
         (discriminating-categories (mapcar #'car (discriminating-categories agent)))
         (used-channels (mapcar #'channel discriminating-categories))
         (unused-channels (set-difference (get-configuration agent :channels) used-channels))
         (new-categories
          (loop for channel in unused-channels
                for topic-value-on-channel = (access-channel (topic agent) channel)
                collect (add-category agent channel topic-value-on-channel))))
    (add-lex agent new-form (append discriminating-categories new-categories))))

;; ------------------
;; + Interpretation +
;; ------------------

(defmethod interpret ((agent mwm-agent) (utterance string))
  "Search for the object in the context that maximizes the overlap
   with the meaning of the utterance (if known)."
  (let ((parsed-lex (find utterance (lexicon agent) :key #'form :test #'string=)))
    (loop with best-object = nil
          with best-overlap = nil
          for object in (entities (context agent))
          for object-categorisation = (let ((categorisation (categorise-object agent object)))
                                        (loop for (channel category similarity) in categorisation
                                              collect (cons category similarity)))
          for overlap = (overlap (meaning lex) object-categorisation)
          when (or (null best-object) (> overlap best-overlap))
          do (setf best-object object
                   best-overlap overlap)
          finally
          (return best-object))))
          
    
    

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
    (set-data (ontology agent) 'channels (list left-category x-middle-category right-category
                                               bottom-category y-middle-category top-category
                                               small-area-category medium-area-category large-area-category
                                               narrow-category medium-width-category wide-category
                                               tall-category medium-height-category short-category
                                               red-category green-category blue-category
                                               shiny-category matte-category
                                               4-sides-category 3-sides-category 10-sides-category
                                               4-corners-category 3-corners-category 0-corners-category
                                               square-ratio-category rectangle-ratio-category))
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