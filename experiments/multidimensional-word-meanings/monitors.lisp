(in-package :mwm)

;; ------------
;; + Monitors +
;; ------------

;;;; A-list monitor to show the convergence of x-pos
(define-monitor record-x-pos-convergence
                :documentation "Record the convergence of x-pos for concepts left, right and green"
                :class 'alist-recorder
                :average-windows 1)

(define-event-handler (record-x-pos-convergence interaction-finished)
  (loop with learner = (find 'learner (population experiment) :key #'id)
        for concept in '("left" "right" "green")
        for cxn = (find concept (constructions (grammar learner))
                        :key #'(lambda (cxn) (attr-val cxn :form))
                        :test #'string=)
        when cxn
        do (let ((x-pos-value (second (find 'x-pos (attr-val cxn :meaning) :key #'first))))
             (set-value-for-symbol monitor
                                   (intern concept)
                                   x-pos-value))))

(define-monitor plot-x-pos-convergence
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-x-pos-convergence
    :average-windows 1
    :draw-y-1-grid t
    :y-label "x-pos value for different concepts"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "x-pos-convergence"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-x-pos-convergence-graph (&key 
                                       (configurations nil)
                                       (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create an x-pos convergence graph." nr-of-interactions)
  (activate-monitor plot-x-pos-convergence)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-x-pos-convergence)
  (format t "~%Graphs have been created"))


;;;; Export lexicon quality (should go to 1)
(defparameter *tutor-lexicon*
  '(("left" (x-pos 0 1))
    ("right" (x-pos 1 1))
    ("front" (y-pos 0 1))
    ("behind" (y-pos 1 1))
    ("large" (area 1 1) (width 1 1) (height 1 1))
    ("small" (area 0 1) (width 0 1) (height 0 1))
    ("rubber" (rgb-variance (0 0 0) 1))
    ("metal" (rgb-variance (1 1 1) 1))
    ("cube" (wh-ratio 1 1) (nr-of-corners 1 1) (nr-of-sides 1 1))
    ("sphere" (wh-ratio 1 1) (nr-of-corners 0 1) (nr-of-sides 0 1))
    ("cylinder" (wh-ratio 0.5 1) (nr-of-corners 0 1) (nr-of-sides 0.4 1))
    ("gray" (mean-rgb (0.34 0.34 0.34) 1))
    ("red" (mean-rgb (0.69 0.14 0.14) 1))
    ("yellow" (mean-rgb (1.0 0.93 0.20) 1))
    ("purple" (mean-rgb (0.51 0.15 0.75) 1))
    ("brown" (mean-rgb (0.51 0.29 0.10) 1))
    ("green" (mean-rgb (0.11 0.41 0.08) 1))
    ("blue" (mean-rgb (0.16 0.29 0.84) 1))
    ("cyan" (mean-rgb (0.16 0.82 0.82) 1))))

(defun lexicon-quality (agent)
  (labels ((1d-sim (x y)
             (- 1 (abs (- x y))))
           (3d-sim (x y)
             (- 1 (/ (euclidean x y)
                     (euclidean '(0 0 0) '(1 1 1))))))
    (loop for cxn in (constructions (grammar agent))
          for form = (attr-val cxn :form)
          for meaning = (attr-val cxn :meaning)
          for tutor-meaning = (rest (assoc form *tutor-lexicon* :test #'string=))
          for v = (loop for (attr value certainty) in meaning
                        for found = (find attr tutor-meaning :key #'first)
                        if found
                        sum (* certainty (if (listp value)
                                           (3d-sim value (second found))
                                           (1d-sim value (second found)))) into cxn-sum
                        else
                        sum 1 into cxn-sum
                        finally (return (float (/ cxn-sum (length tutor-meaning)))))
          collect v into cxn-val
          finally (return (average cxn-val)))))

(define-monitor record-lexicon-quality
                :class 'data-recorder
                :average-window 1
                :documentation "records the lexicon quality (should go to 1).")

(define-monitor display-lexicon-quality
                :class 'gnuplot-display
                :documentation "Plots the lexicon quality."
                :data-sources '((average record-lexicon-quality))
                :update-interval 1
                :caption '("lexicon quality")
                :x-label "# Games" 
                :y1-label "Lexicon Quality" 
                :y1-max nil :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-lexicon-quality
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon quality"
                :data-sources '(record-lexicon-quality)
                :file-name (babel-pathname :name "lexicon-quality" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-quality interaction-finished)
  (record-value monitor (lexicon-quality (hearer interaction))))

;;;; Show learner lexicon
(defun show-learner-lexicon (agent)
  (loop for cxn in (constructions (grammar agent))
        for meaning = (attr-val cxn :meaning)
        do (add-element `((div)
                          ,(s-dot->svg
                            (cxn->s-dot cxn))))))

;;;; Export learner lexicon
(defun export-learner-lexicon (agent)
  (loop for cxn in (constructions (grammar agent))
        for meaning = (attr-val cxn :meaning)
        for strong = (mapcar #'first
                             (find-all-if #'(lambda (certainty) (>= certainty 0.8))
                                          meaning :key #'third))
        for weak = (mapcar #'first
                             (find-all-if #'(lambda (certainty) (< certainty 0.8))
                                          meaning :key #'third))
        do (s-dot->image
            (cxn->s-dot cxn strong weak)
            :path (babel-pathname :directory '(".tmp")
                                  :name (format nil "~a" (attr-val cxn :form))
                                  :type "pdf")
            :format "pdf"
            :open nil)))

;;;; Communicative success
(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 100
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success))
                :update-interval 100
                :caption '("communicative success")
                :x-label "# Games" 
                :y1-label "Communicative Success" 
                :y1-max 1.0 :y1-min 0 
                :draw-y1-grid t)

(define-monitor export-communicative-success
                :class 'lisp-data-file-writer
                :documentation "Exports communicative success"
                :data-sources '(record-communicative-success)
                :file-name (babel-pathname :name "communicative-success" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

#|
(define-event-handler (record-communicative-success interaction-finished)
  (let ((tutor (find 'tutor (population experiment) :key #'id)))
    (record-value monitor
                  (if (discriminative-set tutor)
                    (if (communicated-successfully interaction) 1 0)
                    1))))
|#

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

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
  (length (constructions (grammar agent))))

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (get-lexicon-size (hearer experiment))))

;;;; Number of meanings per form
(define-monitor record-features-per-form
                :class 'data-recorder
                :average-window 100
                :documentation "records avg nr of meanings per form")

(define-monitor export-features-per-form
                :class 'lisp-data-file-writer
                :documentation "Exports nr of meanings per form"
                :data-sources '(record-features-per-form)
                :file-name (babel-pathname :name "features-per-form" :type "lisp"
                                           :directory '("experiments" "multidimensional-word-meanings" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(defun compute-nr-of-features-per-form (agent)
  (loop for cxn in (constructions (grammar agent))
        for meaning = (attr-val cxn :meaning)
        sum (length meaning) into meaning-length-sum
        finally
        (return (if (constructions (grammar agent))
                  (float (/ meaning-length-sum
                            (length (constructions (grammar agent)))))
                  0))))

(define-event-handler (record-features-per-form interaction-finished)
   (record-value monitor (compute-nr-of-features-per-form (hearer experiment))))

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