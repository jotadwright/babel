(in-package :ecl)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod shift-concept ((agent ecl-agent) (topic ecl-object) concept &key weight-incf weight-decf)
  "Shift a concept towards the given topic.

  Shifts 1. the prototype of each feature channel
         2. the certainties of salient channel positively and others negatively."
  ;; 1. update the prototypical values
  (loop for prototype in (meaning concept)
        for interaction-number = (interaction-number (current-interaction (experiment agent)))
        do (update-prototype interaction-number prototype topic (get-configuration agent :prototype-update-strategy)))
  ;; 2. determine which attributes should get an increase
  ;;    in weight, and which should get a decrease.
  (let* ((similarity-table (make-similarity-table agent concept))
         (discriminating-attributes (find-discriminating-attributes agent
                                                                    concept
                                                                    topic
                                                                    similarity-table))
         (all-attribute-subsets (all-subsets (meaning concept)))
         (subsets-to-consider (filter-subsets all-attribute-subsets
                                              discriminating-attributes
                                              (get-configuration agent :alignment-filter)))
         (best-subset (find-most-discriminating-subset agent
                                                       subsets-to-consider
                                                       topic
                                                       similarity-table)))
    (when (null best-subset)
      ;; when best-subset returns NIL
      ;; reward all attributes...
      (setf best-subset (meaning concept)))
    ;; 3. actually update the weight scores
    (loop with rewarded-attributes = nil
          with punished-attributes = nil
          for prototype in (meaning concept)
          if (member (attribute prototype) best-subset :key #'attribute)
            do (progn (push (attribute prototype) rewarded-attributes)
                 (adjust-weight concept
                                   (attribute prototype)
                                   weight-incf
                                   (get-configuration agent :weight-update-strategy)))
          else
            do (progn (push (attribute prototype) punished-attributes)
                 (adjust-weight concept
                                   (attribute prototype)
                                   weight-decf
                                   (get-configuration agent :weight-update-strategy)))
          finally (notify scores-updated concept rewarded-attributes punished-attributes)))) ;; notify
