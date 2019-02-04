(in-package :mwm)

;;;; Interaction script (tutor-learner scenario)
;;;; ------------------

;; Conceptualisation/Production:
;; Find as much discriminating categories for the topic as possible
;; Try to find a form that maximizes overlap using these discriminating categories
;; Perform re-entrance using the found form. Check if this leads to the topic.
;; If not, extend the ontology with discriminating categories on the remaining channels
;; Invent a new form that takes categories on all channels

;; Interpretation:
;; Choose the object that maximizes the similarity to the meaning

;; Adoption:
;; Adopt the utterance together with all the values of the object
;; and low initial certainty (0.05?)

;; Alignment:
;; 2 things need to happen; prototypes shifted (or split or merged??)
;; and certainty scores updated.
;; In case of success or failure:
;;  The channels in the meaning that are very similar to the channels of the object increase in certainty
;;  --> what does it mean to be 'very similar'? Experiment with a threshold value?
;;  Conversly, the channels in the meaning that are dissimilar to the object decrease in certainty
;;  --> How fast should the increase/decrease go? 0.1 vs 0.01?
;; In case of success:
;;  The prototype of the similar meanings can shift, as this interaction showed a positive example.
;; In case of failure:
;;  The hearer can make its meaning more specific. If the object has channel values that the meaning
;;  do not yet have, add them.

(defun initialize-agent (agent context)
  "Initialize the agent"
  (setf (context agent) context
        (topic agent) (first (entities context))
        (utterance agent) nil
        (communicated-successfully agent) nil))

(defmethod interact :before ((experiment mwm-experiment) interaction &key)
  "Generate a context and intialize the agents"
  (let* ((min-context-size (get-configuration experiment :min-context-size))
         (max-context-size (get-configuration experiment :max-context-size))
         (context-size (random-from-range min-context-size max-context-size))
         (context (generate-context context-size)))
    (loop for agent in (interacting-agents interaction)
          do (initialize-agent agent context))))

(defmethod interact ((experiment mwm-experiment) interaction &key)
  nil)

(defmethod interact :after ((experiment mwm-experiment) interaction &key)
  nil)