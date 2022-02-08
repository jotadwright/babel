(in-package :mwm)

;; ------------
;; + Monitors +
;; ------------

;;;; Communicative success
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '(record-communicative-success)
                :update-interval 100
                :caption '("communicative success")
                :x-label "# Games" 
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success))
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Communicative Success given Conceptualisation
(define-monitor record-communicative-success-given-conceptualisation
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor export-communicative-success-given-conceptualisation
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '((average record-communicative-success-given-conceptualisation))
                :file-name (babel-pathname :name "communicative-success-given-conceptualisation" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success-given-conceptualisation interaction-finished)
  (record-value monitor (determine-communicative-success-given-conceptualisation interaction monitor)))

(defun determine-communicative-success-given-conceptualisation (interaction monitor)
  (if (eql (tutor interaction) (speaker interaction))
    (if (communicated-successfully interaction) 1 0)
    (if (find-data (speaker interaction) 'applied-concept)
      (if (communicated-successfully interaction) 1 0)
      (if (caaar (monitors::get-values monitor)) (caaar (monitors::get-values monitor)) 0))))


;;;; Lexicon size
(define-monitor record-lexicon-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun get-lexicon-size (agent)
  (length (lexicon agent)))

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (get-lexicon-size (learner experiment))))


;;;; Monitor that tracks what the tutor uses when the interaction fails
(define-monitor record-tutor-conceptualisation-in-failed-interactions
                :class 'alist-recorder :average-window 1)

(defparameter *used-features* nil)

(define-event-handler (record-tutor-conceptualisation-in-failed-interactions interaction-finished)
  (when (eql (tutor experiment) (speaker experiment))
    (let ((tutor-feature (find-data (tutor experiment) 'tutor-conceptualisation)))
      (unless (communicated-successfully (current-interaction experiment))
        (if (assoc tutor-feature *used-features*)
          (incf (cdr (assoc tutor-feature *used-features*)))
          (push (cons tutor-feature 1) *used-features*)))))
  (loop for (feature . count) in *used-features*
        do (set-value-for-symbol monitor feature count)))

(define-monitor plot-tutor-conceptualisation-in-failed-interactions
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-tutor-conceptualisation-in-failed-interactions
    :draw-y-1-grid t
    :y-label "Tutor word use"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "tutor-word-use" :type "pdf")
    :graphic-type "pdf")

(defun create-tutor-word-use-graph (&key (configurations nil)
                                         (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a tutor word use graph." nr-of-interactions)
  (setf *used-features* nil)
  (activate-monitor plot-tutor-conceptualisation-in-failed-interactions)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-tutor-conceptualisation-in-failed-interactions)
  (format t "~%Graphs have been created"))

;;;; Monitor that tracks what the learner uses when the interaction fails
(define-monitor record-learner-failed-conceptualisations
                :class 'alist-recorder :average-window 1)

(defparameter *failed-conceptualisations* nil)

(define-event-handler (record-learner-failed-conceptualisations interaction-finished)
  (when (eql (learner experiment) (speaker experiment))
    (let ((symbolic-feature (find-data (tutor experiment) 'tutor-conceptualisation)))
      (unless (communicated-successfully (current-interaction experiment))
        (if (assoc symbolic-feature *failed-conceptualisations*)
          (incf (cdr (assoc symbolic-feature *failed-conceptualisations*)))
          (push (cons symbolic-feature 1) *failed-conceptualisations*)))))
  (loop for (feature . count) in *failed-conceptualisations*
        do (set-value-for-symbol monitor feature count)))

(define-monitor plot-learner-failed-conceptualisations
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-learner-failed-conceptualisations
    :draw-y-1-grid t
    :y-label "Learner failed conceptualisation"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "learner-failed-conceptualisation" :type "pdf")
    :graphic-type "pdf")

(defun create-learner-failed-conceptualisation-graph (&key configurations
                                                           (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a graph." nr-of-interactions)
  (setf *failed-conceptualisations* nil)
  (activate-monitor plot-learner-failed-conceptualisations)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-learner-failed-conceptualisations)
  (format t "~%Graphs have been created"))

;;;; Export the lexicon to pdf at the end of a series
(define-monitor export-learner-concepts-to-pdf)

(define-event-handler (export-learner-concepts-to-pdf run-series-finished)
  (lexicon->pdf (learner experiment) :serie (series-number experiment)))


;;;; Export the lexicon to .store at the end of a series
(define-monitor export-learner-concepts-to-store)

(define-event-handler (export-learner-concepts-to-store run-series-finished)
  (lexicon->store (learner experiment) :serie (series-number experiment)))


;;;; Export the configurations of the experiment at the end of the first series
(define-monitor export-experiment-configurations)

(define-event-handler (export-experiment-configurations run-series-finished)
  (when (= (series-number experiment) 1)
    (let* ((experiment-name (experiment-name-from-configurations experiment))
           (path (babel-pathname
                  :directory `("experiments" "multidimensional-word-meanings"
                               "raw-data" ,(downcase experiment-name))
                  :name "experiment-configurations" :type "lisp")))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create)
        (write (entries experiment)
               :stream stream)))))
    















#|
;;;; Utterance length
(define-monitor record-utterance-length
                :class 'data-recorder
                :average-window 1
                :documentation "records the utterance length")

(define-monitor export-utterance-length
                :class 'lisp-data-file-writer
                :documentation "Exports utterance length"
                :data-sources '(record-utterance-length)
                :file-name (babel-pathname :name "utterance-length" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-utterance-length interaction-finished)
  (record-value monitor (length (utterance (speaker interaction)))))


;;;; monitor tutor word use (a-list)
(define-monitor record-tutor-word-use
                :documentation "Record how often the tutor uses each word"
                :class 'alist-recorder
                :average-window 100)

(defparameter *word-count* nil)

(define-event-handler (record-tutor-word-use interaction-finished)
  (let* ((tutor (find 'tutor (population experiment) :key #'id))
         (used-words (find-data tutor 'tutor-conceptualisation)))
    (loop for used-word in used-words
          if (assoc used-word *word-count*)
          do (incf (cdr (assoc used-word *word-count*)))
          else do (push (cons used-word 1) *word-count*))
    (loop for (word . count) in *word-count*
          do (set-value-for-symbol monitor word count))))

(define-monitor plot-tutor-word-use
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-tutor-word-use
    :draw-y-1-grid t
    :y-label "Tutor word use"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "tutor-word-use" :type "pdf")
    :graphic-type "pdf")

(defun create-tutor-word-use-graph (&key (configurations nil)
                                         (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a tutor word use graph." nr-of-interactions)
  (activate-monitor plot-tutor-word-use)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-tutor-word-use)
  (format t "~%Graphs have been created"))


;;;; success per attribute type
(define-monitor record-success-per-attribute-type
                :documentation "For each type of attribute (e.g. color), record the success separately"
                :class 'alist-recorder
                :average-window 100)

(defparameter *word->type-map*
  '(("left" . xpos) ("right" . xpos)
    ("front" . ypos) ("behind" . ypos)
    ("cube" . shape) ("cylinder" . shape) ("sphere" . shape)
    ("metal" . material) ("rubber" . material)
    ("large" . size) ("small" . size)
    ("blue" . color) ("brown" . color) ("cyan" . color)
    ("gray" . color) ("green" . color) ("purple" . color)
    ("red" . color) ("yellow" . color)))

(define-event-handler (record-success-per-attribute-type interaction-finished)
  (let* ((tutor (find 'tutor (population experiment) :key #'id))
         (used-attribute-type (rest (assoc (first (utterance tutor)) *word->type-map* :test #'string=)))
         (success (communicated-successfully interaction)))
    (set-value-for-symbol monitor used-attribute-type (if success 1 0))
    (set-value-for-symbol monitor 'overall (if success 1 0))))

(define-monitor plot-success-per-attribute-type
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-success-per-attribute-type
    :draw-y-1-grid t
    :y-label "Success / attribute type"
    :y-max 1.0
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "success-per-attribute-type"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-success-per-attribute-type-graph (&key (configurations nil)
                                                     (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create an a-list graph." nr-of-interactions)
  (activate-monitor plot-success-per-attribute-type)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-success-per-attribute-type)
  (format t "~%Graphs have been created"))

;;;; Sankey diagram data
;; every 100'th interaction, we export the entire lexicon
;; the columns are: interaction number, word, feature, certainty, value
;; from this csv, we can create a Sankey diagram in Python
(define-monitor record-lexicon-evolution
                :class 'data-recorder
                :average-window 1
                :documentation "records the evolution of the lexicon")

(define-monitor export-lexicon-evolution
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-lexicon-evolution)
                :file-name (babel-pathname :name "lexicon-evolution" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun collect-initial-cxn (cxn)
  (loop with form = (attr-val cxn :form)
        for (category . certainty) in (attr-val cxn :meaning)
        collect (list 0
                      (format nil "\"~a\"" (downcase form))
                      (format nil "\"~a\"" (downcase (mkstr (attribute category))))
                      (format nil "~$" certainty)
                      (format nil "~$" (prototype category)))))

(define-event-handler (record-lexicon-evolution new-cxn-added)
  ;; record the initial representation of a concept
  (let ((lexicon (list (collect-initial-cxn cxn))))
    (record-value monitor lexicon)))

(define-event-handler (record-lexicon-evolution interaction-finished)
  ;; record every 100'th interactions
  (let ((i-nr (interaction-number interaction)))
    (if (= (mod i-nr (get-configuration experiment :dot-interval)) 0)
      (let* ((learner (find 'learner (population experiment) :key #'id))
             (lexicon (loop for cxn in (constructions (grammar learner))
                            for form = (attr-val cxn :form)
                            collect (loop for (category . certainty) in (attr-val cxn :meaning)
                                          collect (list i-nr
                                                        (format nil "\"~a\"" (downcase form))
                                                        (format nil "\"~a\"" (downcase (mkstr (attribute category))))
                                                        (format nil "~2f" certainty)
                                                        (format  nil "~2f" (prototype category)))))))
        (record-value monitor lexicon))
      (unless (listp (current-value monitor))
        (record-value monitor '())))))


;; recording tutor utterance length for bar plots
(define-monitor record-tutor-utterance-length-1
                :class 'data-recorder
                :average-window 1)

(define-monitor export-tutor-utterance-length-1
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-utterance-length-1)
                :file-name (babel-pathname :name "tutor-utterance-length-1" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-utterance-length-1 interaction-finished)
  (let ((tutor-utterance-len (length (utterance (speaker interaction)))))
    (record-value monitor (if (= tutor-utterance-len 1) 1 0))))

(define-monitor record-tutor-utterance-length-2
                :class 'data-recorder
                :average-window 1)

(define-monitor export-tutor-utterance-length-2
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-utterance-length-2)
                :file-name (babel-pathname :name "tutor-utterance-length-2" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-utterance-length-2 interaction-finished)
  (let ((tutor-utterance-len (length (utterance (speaker interaction)))))
    (record-value monitor  (if (= tutor-utterance-len 2) 1 0))))

(define-monitor record-tutor-utterance-length-3
                :class 'data-recorder
                :average-window 1)

(define-monitor export-tutor-utterance-length-3
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-utterance-length-3)
                :file-name (babel-pathname :name "tutor-utterance-length-3" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-utterance-length-3 interaction-finished)
  (let ((tutor-utterance-len (length (utterance (speaker interaction)))))
    (record-value monitor  (if (= tutor-utterance-len 3) 1 0))))

(define-monitor record-tutor-utterance-length-4
                :class 'data-recorder
                :average-window 1)

(define-monitor export-tutor-utterance-length-4
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-utterance-length-4)
                :file-name (babel-pathname :name "tutor-utterance-length-4" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-utterance-length-4 interaction-finished)
  (let ((tutor-utterance-len (length (utterance (speaker interaction)))))
    (record-value monitor  (if (= tutor-utterance-len 4) 1 0))))


;; recording tutor attribute use for bar plots
(define-monitor record-tutor-uses-color
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-color
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-color)
                :file-name (babel-pathname :name "tutor-uses-color" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-color interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'color types) (/ 1 (length utterance)) 0))))

(define-monitor record-tutor-uses-shape
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-shape
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-shape)
                :file-name (babel-pathname :name "tutor-uses-shape" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-shape interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'shape types) (/ 1 (length utterance)) 0))))

(define-monitor record-tutor-uses-material
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-material
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-material)
                :file-name (babel-pathname :name "tutor-uses-material" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-material interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'material types) (/ 1 (length utterance)) 0))))

(define-monitor record-tutor-uses-size
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-size
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-size)
                :file-name (babel-pathname :name "tutor-uses-size" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-size interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'size types) (/ 1 (length utterance)) 0))))

(define-monitor record-tutor-uses-xpos
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-xpos
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-xpos)
                :file-name (babel-pathname :name "tutor-uses-xpos" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-xpos interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'xpos types) (/ 1 (length utterance)) 0))))

(define-monitor record-tutor-uses-ypos
                :class 'data-recorder :average-window 1)

(define-monitor export-tutor-uses-ypos
                :class 'lisp-data-file-writer
                :data-sources '(record-tutor-uses-ypos)
                :file-name (babel-pathname :name "tutor-uses-ypos" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil)

(define-event-handler (record-tutor-uses-ypos interaction-finished)
  (let* ((utterance (utterance (speaker interaction)))
         (types (loop for word in utterance
                      for type = (rest (assoc word *word->type-map* :test #'string=))
                      collect type)))
    (record-value monitor (if (find 'ypos types) (/ 1 (length utterance)) 0))))


;;;; Record reason for failure
;; a-list monitor that keeps track of the reason why the game failed
(define-monitor record-game-outcome
                :documentation "records in a few symbols the game outcome"
                :class 'alist-recorder
                :average-window 100)

(define-event-handler (record-game-outcome interaction-finished)
  (let ((learner (find 'learner (population experiment) :key #'id))
        (tutor (find 'tutor (population experiment) :key 'id)))
    (if (communicated-successfully interaction)
      (set-value-for-symbol monitor 'success 1)
      ;; if the tutor speaks
      ;; - either the word is new for the learner
      ;; - either the interpreted object is wrong
      ;; if the learner speaks
      ;; - either the learner cannot discriminate the topic
      ;; - either the tutor cannot interpret the utterance
      ;; - either the agents do not agree
      (set-value-for-symbol monitor
                            (if (speakerp tutor)
                              (cond ((null (find-data learner 'parsed-meaning)) 'new-word-for-learner)
                                    (t 'agents-not-agree))
                              (cond ((null (find-data learner 'applied-cxns)) 'not-discriminate)
                                    ((null (find-data tutor 'interpreted-topic)) 'tutor-not-interpret)
                                    (t 'agents-not-agree)))
                            1))))

(define-monitor plot-game-outcomes
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-game-outcome
    :draw-y-1-grid t
    :y-label "Game outcomes"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "game-outcomes"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-game-outcome-graph (&key
                                  (configurations nil)
                                  (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a game outcome graph." nr-of-interactions)
  (activate-monitor plot-game-outcomes)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-game-outcomes)
  (format t "~%Graphs have been created"))


;;;; Learner used attribute
;; a-list monitor that keeps track of the attributes used by the learner
(define-monitor record-learner-attribute-use
                :documentation "Record how often the learner uses each type of attribute"
                :class 'alist-recorder
                :average-window 100)

(defparameter *attribute-count* nil)

(define-event-handler (record-learner-attribute-use interaction-finished)
  (let ((learner (find 'learner (population experiment) :key #'id)))
    (when (find-data learner 'parsed-meaning)
      (loop for (category . score) in (find-data learner 'parsed-meaning)
            for attr = (attribute category)
            if (> score 0.0)
            do (if (assoc attr *attribute-count*)
                 (incf (cdr (assoc attr *attribute-count*)))
                 (push (cons attr 1) *attribute-count*))))
    (loop for (attr . count) in *attribute-count*
          do (set-value-for-symbol monitor attr count))))

(define-monitor plot-learner-attribute-use
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-learner-attribute-use
    :draw-y-1-grid t
    :y-label "Learner attribute use"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "learner-attribute-use"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-learner-attribute-use-graph (&key 
                                           (configurations nil)
                                           (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a learner attribute use graph." nr-of-interactions)
  (activate-monitor plot-learner-attribute-use)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-learner-attribute-use)
  (format t "~%Graphs have been created"))
|#
