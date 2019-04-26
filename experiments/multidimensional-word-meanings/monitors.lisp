(in-package :mwm)

;; ------------
;; + Monitors +
;; ------------

(defun lexicon->function-plots (agent)
  (loop for cxn in (constructions (grammar agent)) 
        for experiment-name = (list-of-strings->string (list (downcase (mkstr (get-configuration agent :category-representation)))
                                                             (downcase (mkstr (get-configuration agent :alignment-strategy))))
                                                       :separator "-")
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
               collect (format nil "box(x,~a,~a)"
                               (lower-bound category)
                               (upper-bound category)))))
    (create-function-plot equations
                          :function-definitions '("box(x,a,b)=(x>a && x<b) ? 1 : -1")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
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
                          :x-label nil :y-min -0.1
                          :open nil)))

#|
(defmethod cxn->function-plot ((cxn fcg-construction) (category-representation (eql :exponential))
                                &key (directory '(".tmp")))
  (let ((equations
         (loop for (category . certainty) in (attr-val cxn :meaning)
               append (list (format nil "decay(x,~a)"
                                    (left-sigma category))
                            (format nil "decay(x,~a)"
                                    (- (right-sigma category)))))))
    (create-function-plot equations
                          :function-definitions '("decay(x,s)=exp(s*x)")
                          :title (format nil "~a" (downcase (mkstr (name cxn))))
                          :captions (loop for (category . certainty) in (attr-val cxn :meaning)
                                          append (list (format nil "~a-lower" (downcase (mkstr (attribute category))))
                                                       (format nil "~a-upper" (downcase (mkstr (attribute category))))))
                          :plot-file-name (format nil "~a" (downcase (mkstr (name cxn))))
                          :plot-directory directory
                          :x-label nil :y-min 0 :x-min -0.5 :x-max 0.5
                          :open nil)))
|#


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

;;;; Tutor used attribute
;; a-list monitor that keeps track of the attribute used by the tutor
;; (color, shape, size, material, x-pos or y-pos)
(define-monitor record-tutor-attribute-use
                :documentation "Record how often the tutor uses each type of attribute"
                :class 'alist-recorder
                :average-windows 1)

(defvar word->attr
  '((left . x-pos) (right . x-pos)
    (front . y-pos) (behind . y-pos)
    (large . size) (small . size)
    (rubber . material) (metal . material)
    (cube . shape) (sphere . shape) (cylinder . shape)
    (gray . color) (red . color) (yellow . color) (purple . color)
    (brown . color) (green . color) (blue . color) (cyan . color)))

(define-event-handler (record-tutor-attribute-use interaction-finished)
  (let* ((tutor (find 'tutor (population experiment) :key #'id))
         (used-attr (rest (assoc (first (discriminative-set tutor)) word->attr))))
    (loop for attr in '(x-pos y-pos size material shape color)
          if (eql attr used-attr)
          do (set-value-for-symbol monitor attr 1)
          else
          do (set-value-for-symbol monitor attr 0))))

(define-monitor plot-tutor-attribute-use
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-tutor-attribute-use
    :average-windows 1
    :draw-y-1-grid t
    :y-label "how often each attribute type is used"
    :x-label "# Games"
    :file-name (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "graphs")
			       :name "tutor-attribute-use"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-tutor-attribute-use-graph (&key 
                                       (configurations nil)
                                       (nr-of-interactions 5000))
  (format t "~%Running ~a interactions in order to create a tutor attribute use graph." nr-of-interactions)
  (activate-monitor plot-tutor-attribute-use)
  (run-batch 'mwm-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-tutor-attribute-use)
  (format t "~%Graphs have been created"))

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
        do (add-element `((div)
                          ,(s-dot->svg
                            (cxn->s-dot cxn))))))

;;;; Export learner lexicon
(defun export-learner-lexicon (agent &key (experiment-name 'baseline))
  (let ((base-path (babel-pathname :directory `("experiments" "multidimensional-word-meanings"
                                                "graphs" ,(downcase (mkstr experiment-name)) "lexicon"))))
    (ensure-directories-exist base-path)
    (loop for cxn in (constructions (grammar agent))
          do (s-dot->image
              (cxn->s-dot cxn)
              :path (merge-pathnames (make-pathname :name (format nil "~a-cxn" (attr-val cxn :form)) :type "pdf")
                                     base-path)
              :format "pdf"
              :open nil))))

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