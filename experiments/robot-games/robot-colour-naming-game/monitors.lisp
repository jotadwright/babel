(in-package :gcng)

;;;; Communicative Success

(define-monitor record-communicative-success
                :class 'data-recorder
                :average-window 50
                :documentation "records the game outcome of each game (1 or 0).")

(define-monitor display-communicative-success
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success))
                :update-interval 50
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
                                           :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-communicative-success interaction-finished)
  (record-value monitor (if (communicated-successfully interaction) 1 0)))

;;;; Lexicon Size

(define-monitor record-lexicon-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the avg lexicon size.")

(define-monitor export-lexicon-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-lexicon-size)
                :file-name (babel-pathname :name "lexicon-size" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-lexicon-size interaction-finished)
  (record-value monitor (population-average-lexicon-size (population experiment))))

(defun population-average-lexicon-size (population)
  (loop for agent in population
        sum (length (constructions (grammar agent))) into length-sum
        finally
        (return (float (/ length-sum (length population))))))

;;;; Ontology Size

(define-monitor record-ontology-size
                :class 'data-recorder
                :average-window 1
                :documentation "records the avg lexicon size.")

(define-monitor export-ontology-size
                :class 'lisp-data-file-writer
                :documentation "Exports lexicon size"
                :data-sources '(record-ontology-size)
                :file-name (babel-pathname :name "ontology-size" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-ontology-size interaction-finished)
  (record-value monitor (population-average-ontology-size (population experiment))))

(defmethod population-average-ontology-size (population)
  (loop for agent in population
        for colors = (get-data (ontology agent) 'color-categories)
        sum (length colors) into length-sum
        finally
        (return (float (/ length-sum (length population))))))

;;;; Avg number of forms per meaning

(define-monitor record-avg-forms-per-meaning
                :class 'data-recorder
                :average-window 1
                :documentation "records avg nr of forms per meaning")

(define-monitor export-avg-forms-per-meaning
                :class 'lisp-data-file-writer
                :documentation "Exports nr of forms per meaning"
                :data-sources '(record-avg-forms-per-meaning)
                :file-name (babel-pathname :name "forms-per-meaning" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-avg-forms-per-meaning interaction-finished)
   (record-value monitor (compute-avg-nr-of-forms-per-meaning (population experiment))))

(defun compute-avg-nr-of-forms-per-meaning (population)
  ;; for each agent:
  ;;   go over all meanings and count the number of forms per meaning
  ;;   divide the lexicon size by this number
  ;;   sum this
  ;; divide by the population size
  (loop for agent in population
        for agent-val = (loop with meaning-count = nil
                              for cxn in (constructions (grammar agent))
                              for color-id = (attr-val cxn :category)
                              for found = (assoc color-id meaning-count)
                              if found do (incf (cdr found))
                              else do (push (cons color-id 1) meaning-count)
                              finally
                              (return (average (mapcar #'cdr meaning-count))))
        sum agent-val into agent-sum
        finally
        (return (float (/ agent-sum (length population))))))

;;;; Avg number of meanings per form

(define-monitor record-avg-meanings-per-form
                :class 'data-recorder
                :average-window 1
                :documentation "records avg nr of meanings per form")

(define-monitor export-avg-meanings-per-form
                :class 'lisp-data-file-writer
                :documentation "Exports nr of meanings per form"
                :data-sources '(record-avg-meanings-per-form)
                :file-name (babel-pathname :name "meanings-per-form" :type "lisp"
                                           :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data"))
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-event-handler (record-avg-meanings-per-form interaction-finished)
   (record-value monitor (compute-avg-nr-of-meanings-per-form (population experiment))))

(defun compute-avg-nr-of-meanings-per-form (population)
  ;; for each agent:
  ;;   go over all forms and count the number of meanings per form
  ;;   divide the lexicon size by this number
  ;;   sum this
  ;; divide by the population size
  (loop for agent in population
        for agent-val = (loop with form-count = nil
                              for cxn in (constructions (grammar agent))
                              for form = (attr-val cxn :form)
                              for found = (assoc form form-count :test #'string=)
                              if found do (incf (cdr found))
                              else do (push (cons form 1) form-count)
                              finally
                              (return (average (mapcar #'cdr form-count))))
        sum agent-val into agent-sum
        finally
        (return (float (/ agent-sum (length population))))))

;;;; Meaning-form competition

(define-monitor record-meaning-form-competition
                :documentation "Record meaning-form competition across the population"
                :class 'alist-recorder
                :average-windows 1)

(define-event-handler (record-meaning-form-competition interaction-finished)
  (let ((forms-and-scores (compute-meaning-form-competition experiment)))
    (loop for (form . score) in forms-and-scores
          do (set-value-for-symbol monitor (intern form) score))))

(defparameter *meaning-to-track* nil)

(defun compute-meaning-form-competition (experiment)
  (unless *meaning-to-track*
    (let ((speaker-colors (get-data (ontology (speaker experiment)) 'color-categories)))
      (setf *meaning-to-track* (id (random-elt speaker-colors)))))
  (loop with form-scores = nil
        for agent in (population experiment)
        do (loop for cxn in (constructions (grammar agent))
                 for form = (attr-val cxn :form)
                 for meaning = (attr-val cxn :category)
                 for score = (attr-val cxn :score)
                 for found = (assoc form form-scores :test #'string=)
                 when (eql meaning *meaning-to-track*)
                 if found do (push score (rest found))
                 else do (push (list form score) form-scores))
        finally
        (return (loop for item in form-scores
                      collect (cons (first item) (average (rest item)))))))

(define-monitor plot-meaning-form-competition
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-meaning-form-competition
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Forms associated to one color category"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data")
			       :name "meaning-form-competition"
			       :type "pdf")
    :graphic-type "pdf")

;;;; Form-meaning competition

(define-monitor record-form-meaning-competition
                :documentation "Record form-meaning competition across the population"
                :class 'alist-recorder
                :average-windows 1)

(define-event-handler (record-form-meaning-competition interaction-finished)
  (let ((meanings-and-scores (compute-form-meaning-competition experiment)))
    (loop for (meaning . score) in meanings-and-scores
          do (set-value-for-symbol monitor (intern meaning) score))))

(defparameter *form-to-track* nil)

(defun compute-form-meaning-competition (experiment)
  (unless *form-to-track*
    (let ((speaker-forms (mapcar #'(lambda (cxn) (attr-val cxn :form))
                                 (constructions (grammar (speaker experiment))))))
      (setf *form-to-track* (random-elt speaker-forms))))
  (loop with meaning-scores = nil
        for agent in (population experiment)
        do (loop for cxn in (constructions (grammar agent))
                 for form = (attr-val cxn :form)
                 for meaning = (attr-val cxn :category)
                 for score = (attr-val cxn :score)
                 for found = (assoc meaning meaning-scores)
                 when (string= form *form-to-track*)
                 if found do (push score (rest found))
                 else do (push (list meaning score) meaning-scores))
        finally
        (return (loop for item in meaning-scores
                      collect (cons (first item) (average (rest item)))))))

(define-monitor plot-form-meaning-competition
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-form-meaning-competition
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Meanings associated to one form"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "robot-games" "robot-colour-naming-game" "graphs")
			       :name "form-meaning-competition"
			       :type "pdf")
    :graphic-type "pdf")

;;;; Export color lexicon

(define-monitor export-color-lexicon)

(define-event-handler (export-color-lexicon interaction-finished)
  (export-color-lexicon experiment interaction))

(defun export-color-lexicon (experiment interaction)
  (let ((s-number (series-number experiment))
        (i-number (interaction-number interaction))
        (interval (get-configuration experiment :export-lexicon-interval)))
    (when (= (mod i-number interval) 0)
      (loop for agent in (population experiment)
            do (lexicon->pdf agent i-number s-number)))))

;;;; Disconnect robots after series

(define-monitor disconnect-robots-after-series)

(define-event-handler (disconnect-robots-after-series run-series-finished)
  (loop for robot in (robots experiment)
        do (disconnect-robot robot)))

(define-monitor display-metrics
                :class 'gnuplot-display
                :documentation "Plots the communicative success."
                :data-sources '((average record-communicative-success)
                                (average record-lexicon-size)
                                (average record-ontology-size)
                                (average record-avg-forms-per-meaning)
                                (average record-avg-meanings-per-form))
                :update-interval 50
                :caption '("communicative success"
                           "lexicon size"
                           "ontology size"
                           "forms per meaning"
                           "meanings per form")
                :x-label "# Games"
                :use-y-axis '(1 2 2 2 2)
                :y1-label "Success"
                :y2-label "Size"
                :y1-max 1.0 :y1-min 0
                :y2-max nil :y1-min 0
                :draw-y1-grid t)