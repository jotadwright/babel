(in-package :mwm)

;; ------------
;; + Monitors +
;; ------------

;;export learner lexicon
(define-monitor export-lexicon-evolution)

(define-event-handler (export-lexicon-evolution interaction-finished)
  (export-lexicon-evolution experiment interaction))

(defun export-lexicon-evolution (experiment interaction)
  (let ((i-number (interaction-number interaction))
        (interval (get-configuration experiment :export-lexicon-interval)))
    (when (= (mod i-number interval) 0)
      (let ((learner (find 'learner (population experiment) :key #'id)))
        (lexicon->pdf learner :experiment-name
                        (list-of-strings->string (list (mkstr (get-configuration experiment :category-representation))
                                                       (mkstr i-number))
                                                 :separator "-"))))))

;;;; Show lexicon in web interface
(defun display-lexicon (agent)
  (loop for cxn in (constructions (grammar agent))
        do (add-element `((div)
                          ,(s-dot->svg
                            (cxn->s-dot cxn))))
        do (add-element '((hr)))))

;;;; Export learner lexicon to pdf
(defun experiment-name-from-configurations (experiment)
  (downcase
   (string-append (get-configuration experiment :experiment-type) "-"
                  (get-configuration experiment :data-type) "-"
                  (get-configuration experiment :category-representation) "-"
                  (if (eql (get-configuration experiment :experiment-type) :cogent)
                    (string-append "train-" (mkstr (get-configuration experiment :switch-conditions-after-n-interactions)) "-") "")
                  (if (eql (get-configuration experiment :experiment-type) :incremental)
                    (string-append "condition-" (mkstr (uiop:last-char (first (get-configuration experiment :data-sets)))) "-") "")
                  "lexicon")))

(defun lexicon->pdf (agent &key name)
  (let* ((experiment-name (if name name (experiment-name-from-configurations (experiment agent))))
         (base-path (babel-pathname :directory `("experiments" "multidimensional-word-meanings"
                                                 "graphs" ,(downcase experiment-name)))))
    (ensure-directories-exist base-path)
    (loop for json-cxn in (average-over-cxn-history agent)
          do (s-dot->image
              (json-cxn->s-dot json-cxn)
              :path (merge-pathnames (make-pathname :name (format nil "~a-cxn" (rest (assoc :form json-cxn))) :type "pdf") base-path)
              :format "pdf"
              :open nil))))

;; lexicon -> function plots
(defun lexicon->function-plots (agent)
  (loop for cxn in (constructions (grammar agent)) 
        for experiment-name = (downcase (mkstr (get-configuration agent :category-representation)))
        do (cxn->function-plot cxn (get-configuration agent :category-representation)
                               :directory `("experiments" "multidimensional-word-meanings"
                                            "graphs" ,experiment-name "function-plots"))))

;;;; Export concepts (= cxns) as functions
(defgeneric cxn->function-plot (cxn category-representation &key directory)
  (:documentation "Plot the functions that are stored in the categories"))

(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :min-max))
                               &key (directory '(".tmp")))
  (let ((equations
         (loop for (category . certainty) in (attr-val cxn :meaning)
               when (< (abs (- (lower-bound category) (upper-bound category))) 0.5)
               collect (format nil "box(x,~a,~a)"
                               (lower-bound category)
                               (upper-bound category)))))
    (create-function-plot equations
                          :function-definitions '("box(x,a,b)=(x>a && x<b) ? 1 : -1")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          when (< (abs (- (lower-bound category) (upper-bound category))) 0.5)
                                          collect (format nil "~a" (downcase (mkstr (attribute category)))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min -1.1 :y-max 1.1
                          :open nil)))

(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :prototype))
                                &key (directory '(".tmp")))
  (let ((equations
         (loop for (category . certainty) in (attr-val cxn :meaning)
               collect (format nil "normal(x,~a,~a)"
                               (prototype category)
                               (sqrt (/ (M2 category) (nr-samples category)))))))
    (create-function-plot equations
                          :function-definitions '("normal(x,mu,sd) = (1/(sd*sqrt(2*pi)))*exp(-(x-mu)**2/(2*sd**2))")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          collect (format nil "~a" (downcase (mkstr (attribute category)))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min 0 :y-max nil
                          :open nil)))

(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :prototype-min-max))
                                &key (directory '(".tmp")))
  (let ((equations
         (loop for (category . certainty) in (attr-val cxn :meaning)
               append (list (if (null (lower-m category))
                              (format nil "box(x,~a)"
                                      (prototype category))
                              (format nil "lin(x,~a,~a)"
                                      (lower-m category) (lower-b category)))
                            (if (null (upper-m category))
                              (format nil "box(x,~a)"
                                      (prototype category))
                              (format nil "lin(x,~a,~a)"
                                      (upper-m category) (upper-b category)))))))
    (create-function-plot equations
                          :function-definitions '("lin(x,m,b) = m*x+b"
                                                  "box(x,a) = (x > a-0.1 && x < a+0.1) ? 1 : -1")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          append (list (format nil "~a-lower" (downcase (mkstr (attribute category))))
                                                       (format nil "~a-upper" (downcase (mkstr (attribute category))))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min 0
                          :open nil)))


(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :exponential))
                               &key (directory '(".tmp")))
  (let* ((p-values
          (loop for (category . certainty) in (attr-val cxn :meaning)
                append (list (sqrt (/ 1.0 (expt 2 (/ (* (- (left-sigma category)) (prototype category)) (log 2)))))
                             (sqrt (/ 1.0 (expt 2 (/ (* (- (right-sigma category)) (prototype category)) (log 2))))))))
         (equations
          (loop for (category . certainty) in (attr-val cxn :meaning)
                for i from 0
                append (list (format nil "decay(x,~a,~a)"
                                     (left-sigma category)
                                     (- (nth (* 2 i) p-values)))
                             (format nil "decay(x,~a,~a)"
                                     (- (right-sigma category))
                                     (nth (+ (* 2 i) 1) p-values))))))
    (create-function-plot equations
                          :function-definitions '("decay(x,s,p)=2**(((s*x)/log(2))+p)")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          append (list (format nil "~a-lower" (downcase (mkstr (attribute category))))
                                                       (format nil "~a-upper" (downcase (mkstr (attribute category))))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min 0
                          :open nil)))

;;;; cxn -> json
(defgeneric cxn->json (cxn category-representation)
  (:documentation "Export the cxn to json such that it can be used later"))

(defmethod cxn->json ((cxn fcg-construction) (category-representation (eql :min-max)))
  `((:form . ,(attr-val cxn :form))
    (:meaning . ,(loop for (category . certainty) in (attr-val cxn :meaning)
                       collect `((:attribute . ,(mkstr (attribute category)))
                                 (:lower--bound . ,(lower-bound category))
                                 (:upper--bound . ,(upper-bound category))
                                 (:certainty . ,certainty))))
    (:type . ,(mkstr (type-of (car (first (attr-val cxn :meaning))))))))

(defmethod cxn->json ((cxn fcg-construction) (category-representation (eql :prototype)))
  `((:form . ,(attr-val cxn :form))
    (:meaning . ,(loop for (category . certainty) in (attr-val cxn :meaning)
                       collect `((:attribute . ,(mkstr (attribute category)))
                                 (:prototype . ,(prototype category))
                                 (:nr--samples . ,(nr-samples category))
                                 (:M2 . ,(M2 category))
                                 (:certainty . ,certainty))))
    (:type . ,(mkstr (type-of (car (first (attr-val cxn :meaning))))))))

(defmethod cxn->json ((cxn fcg-construction) (category-representation (eql :prototype-min-max)))
  `((:form . ,(attr-val cxn :form))
    (:meaning . ,(loop for (category . certainty) in (attr-val cxn :meaning)
                       collect `((:attribute . ,(mkstr (attribute category)))
                                 (:lower--bound . ,(lower-bound category))
                                 (:upper--bound . ,(upper-bound category))
                                 (:prototype . ,(prototype category))
                                 (:nr--samples . ,(nr-samples category))
                                 (:M2 . ,(M2 category))
                                 (:lower--m . ,(lower-m category))
                                 (:lower--b . ,(lower-b category))
                                 (:upper--m . ,(upper-m category))
                                 (:lower--b . ,(lower-b category))
                                 (:certainty . ,certainty))))
    (:type . ,(mkstr (type-of (car (first (attr-val cxn :meaning))))))))

(defmethod cxn->json ((cxn fcg-construction) (category-representation (eql :exponential)))
  `((:form . ,(attr-val cxn :form))
    (:meaning . ,(loop for (category . certainty) in (attr-val cxn :meaning)
                       collect `((:attribute . ,(mkstr (attribute category)))
                                 (:prototype . ,(prototype category))
                                 (:nr--samples . ,(nr-samples category))
                                 (:M2 . ,(M2 category))
                                 (:left--sigma . ,(left-sigma category))
                                 (:right--sigma . ,(right-sigma category))
                                 (:certainty . ,certainty))))
    (:type . ,(mkstr (type-of (car (first (attr-val cxn :meaning))))))))

;;;; Learner used attribute
;; a-list monitor that keeps track of the attributes used by the learner
(define-monitor record-learner-attribute-use
                :documentation "Record how often the learner uses each type of attribute"
                :class 'alist-recorder
                :average-window 100)

(defparameter *attribute-count* nil)

(define-event-handler (record-learner-attribute-use interaction-finished)
  (let ((learner (find 'learner (population experiment) :key #'id)))
    (when (parsed-meaning learner)
      (loop for (category . score) in (parsed-meaning learner)
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

;;;; monitor tutor word use (a-list)
(define-monitor record-tutor-word-use
                :documentation "Record how often the tutor uses each word"
                :class 'alist-recorder
                :average-window 100)

(defparameter *word-count* nil)

(define-event-handler (record-tutor-word-use interaction-finished)
  (let* ((tutor (find 'tutor (population experiment) :key #'id))
         (used-word (first (discriminative-set tutor))))
    (if (assoc used-word *word-count*)
      (incf (cdr (assoc used-word *word-count*)))
      (push (cons used-word 1) *word-count*))
    (loop for (word . count) in *word-count*
          do (set-value-for-symbol monitor word count))))

(define-monitor plot-tutor-word-use
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-tutor-word-use
    :draw-y-1-grid t
    :y-label "Tutor word use"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "tutor-word-use"
			       :type "pdf")
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
    (set-value-for-symbol monitor used-attribute-type
                          (if success 1 0))
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