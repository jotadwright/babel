(in-package :cle)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod shift-concept ((agent cle-agent) (topic cle-object) (concept concept))
  "Shift a concept towards the given topic.

  Shifts 1. the prototype of each feature channel
         2. the certainties of salient channel positively and others negatively."
  (let ((prototypes (get-available-prototypes agent concept)))
    ;; 1. update the prototypical values
    (loop for prototype in prototypes ;; assumes prototype
          for interaction-number = (interaction-number (current-interaction (experiment agent)))
          for new-observation = (perceive-object-val agent topic (channel prototype))
          do (update-prototype new-observation
                               interaction-number
                               prototype
                               :save-distribution-history (get-configuration (experiment agent) :save-distribution-history)))
  
    ;; 2. determine which attributes should get an increase
    ;;    in weight, and which should get a decrease.
    (let* ((discriminating-prototypes (find-discriminating-features agent concept topic prototypes)))
      (when (null discriminating-prototypes)
        ;; when best-subset returns NIL
        ;; reward all attributes...
        (setf discriminating-prototypes prototypes))
      ;; 3. actually update the weight scores
      (loop for prototype in prototypes
            ;; if part of the contributing prototypes -> reward
            if (member (channel prototype) discriminating-prototypes :key #'channel)
              do (update-weight prototype
                                (get-configuration (experiment agent) :weight-incf)
                                (get-configuration (experiment agent) :weight-update-strategy))
            ;; otherwise -> punish
            else
              do (update-weight prototype
                                (get-configuration (experiment agent) :weight-decf)
                                (get-configuration (experiment agent) :weight-update-strategy))))))


;; ------------------------------------
;; + find the discriminating channels +
;; ------------------------------------
(defun find-discriminating-features (agent concept topic prototypes)
  "Find all attributes that are discriminating for the topic."
  (loop with context = (remove topic (objects (get-data agent 'context)))
        with discriminating-attributes = nil
        for prototype in prototypes
        for channel = (channel prototype)
        for topic-sim = (distribution-feature-similarity (perceive-object-val agent topic channel) prototype)
        for best-other-sim = (loop for object in context
                                   for observation = (perceive-object-val agent object channel)
                                   maximize (distribution-feature-similarity observation prototype))
        when (> topic-sim best-other-sim)
          do (push prototype discriminating-attributes)
        finally (return discriminating-attributes)))
