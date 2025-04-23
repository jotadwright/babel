;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; Script for PLOTTING ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cgl)
(in-package :cgl)

;;;; UTILS FOR PLOTTING
;;;; ------------------

(defun create-graph-for-single-strategy (experiment-name measure-names
                                         &rest evo-plot-keyword-args)
  ;; take some arguments, but pass along the rest to raw-files->evo-plot
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (let* ((raw-file-paths
          (loop for measure-name in measure-names
                collect `("experiments" "clevr-grammar-learning" "raw-data" ,experiment-name ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "clevr-grammar-learning" "graphs" ,experiment-name)
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))

(defun create-graph-comparing-strategies (&key experiment-names measure-name (average-windows 100)
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions title open start end)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows average-windows
    :captions (if captions captions experiment-names)
    :title (if title title "")
    :plot-directory '("experiments" "clevr-learning" "graphs")
    :error-bars '(:percentile 5 95)
    :error-bar-modes '(:filled)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open
    :start start :end end)
  (format t "~%Graphs have been created"))

#|
(defun create-graph-for-single-strategy (&key experiment-name measure-names (average-windows 100)
                                              y-axis (y1-min 0) y1-max y2-max xlabel y1-label y2-label
                                              captions open points series-numbers end key-location)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows average-windows
    :plot-directory `("experiments" "clevr-learning" "graphs" ,experiment-name)
    :error-bars '(:percentile 5 95)
    :error-bar-modes '(:filled)
    :captions captions
    :use-y-axis y-axis
    :y1-min y1-min
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :points points
    :series-numbers series-numbers
    :end end
    :open open
    :key-location key-location)
  (format t "~%Graphs have been created"))
|#




;;;

(create-all-graphs-for-single-experiment "th-link-mixed-mode-random-tutor")

(create-all-graphs-comparing-experiment '("th-link-mixed-mode-random-tutor"
                                          "th-link-mixed-mode-smart-tutor")
                                        '("baseline tutor"
                                          "probabilistic tutor"))

(create-graph-for-single-strategy 
 "th-link-mixed-mode-random-tutor"
 '("communicative-success" "lexicon-size")
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max nil
 :x-label "Number of Communicative Interactions"
 :y1-label "Communicative Success"
 :y2-label "Construction Inventory Size"
 :captions '("communicative success" "construction inventory size")
 :open t :logscale "x")

(create-graph-for-single-strategy
 :experiment-name "th-link-mixed-mode-random-tutor"
 :measure-names '("number-of-holophrase-cxns"
                  "number-of-item-based-cxns"
                  "number-of-lexical-cxns")
 :average-windows '(1 1 1)
 :y-axis '(1) :y1-min 0 :y1-max nil
 :xlabel "Number of Games"
 :y1-label "Number of Constructions"
 :captions '("holophrase" "item-based" "lexical")
 :end 10000 :open nil)  

(create-graph-for-single-strategy
 :experiment-name "th-link-mixed-mode-random-tutor"
 :measure-names '("repair-add-holophrase"
                  "repair-holophrase-to-item-based"
                  "repair-lexical-to-item-based"
                  "repair-item-based-to-lexical"
                  "repair-add-th-links")
 :average-windows '(250 250 250 250 250)
 :y-axis '(1 1 1 1 1) :y1-max 0.5
 :xlabel "Number of Games"
 :y1-label "Active in Fraction of Games"
 :captions '("learn holophrase" "generalise over holophrase"
             "partial analysis (lexical)"
             "partial analysis (item-based)"
             "extend categorial network")
 :open nil)

(create-graph-for-single-strategy
   :experiment-name "100k-composer-strategy-store-scenes-random-tutor"
   :measure-names '("failed-formulation-th-link"
                    "successful-formulation-th-link")
   :average-windows 1
   :y-axis '(1 1) :y1-min 0 :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Success"
   :open nil)

(loop for experiment in '(default-configurations
                          cxn-supplier-with-scores
                          random-tutor-sample-mode)
      ;do (loop for i below 10
      do (raw-files->evo-plot
          :raw-file-paths
          `(("experiments" "clevr-learning" "raw-data" "latest"
             ,(downcase (mkstr experiment)) "communicative-success")
            ("experiments" "clevr-learning" "raw-data" "latest"
             ,(downcase (mkstr experiment)) "lexicon-size"))
          ;:title (format nil "Series ~a" i)
          :captions '("communicative success" "grammar size")
          :average-windows 1000
          ;:plot-file-name (format nil "series-~a" i)
          ;:plot-directory `("experiments" "clevr-learning" "raw-data"
          ;                  ,(downcase (mkstr experiment)) "per-series")
          ;:series-numbers (list i)
          :use-y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-min 0
          :y1-label "Communicative Success"
          :y2-label "Grammar Size"
          :open nil))

(defun create-all-graphs (experiment-name)
  (create-graph-for-single-strategy :experiment-name experiment-name
                                    :measure-names '("communicative-success"
                                                     "lexicon-size")
                                    :y-axis '(1 2) :y1-max 1 :open nil))

(create-all-graphs "test-smart-speaker-learner")

(create-graph-comparing-strategies
 :experiment-names '("0.4-250k-composer-strategy-store-scenes-random-tutor"
                     "0.4-250k-composer-strategy-store-scenes-smart-tutor")
 :measure-name "number-of-item-based-cxns"
 :y1-label "Number of Item-Based Constructions"
 :captions '("baseline tutor" "probabilistic tutor")
 :average-windows 1
 :y-min 0 :y-max nil
 :xlabel "Number of Games"
 ;:end 25000
 :open nil)

(create-graph-comparing-strategies
 :experiment-names '("v1-composer-strategy-store-scenes-smart-tutor"
                     "v2-composer-strategy-store-scenes-smart-tutor"
                     "v3-composer-strategy-store-scenes-smart-tutor"
                     "v4-composer-strategy-store-scenes-smart-tutor")
 :measure-name "lexicon-size" :y1-label "Grammar Size"
 :captions '("v1" "v2" "v3" "v4") :y-max nil
 :xlabel "Number of Games" :open nil)

(create-graph-comparing-strategies
 :experiment-names (mapcar #'downcase
                           (mapcar #'mkstr
                                   '(cxn-decf-0.2-confidence-0.2
                                     cxn-decf-0.2-confidence-0.3
                                     cxn-decf-0.3-confidence-0.1
                                     cxn-decf-0.3-confidence-0.3
                                     cxn-decf-0.3-confidence-0.4)))
 :measure-name "communicative-success"
 :y1-label "Communicative Success"
 :title "Effect of :cxn-decf-score and :learner-speaks-confidence-level"
 :y-min 0.95 :y-max 1 :open t)

(create-graph-comparing-strategies
 :experiment-names (mapcar #'downcase
                           (mapcar #'mkstr
                                   '(cxn-decf-0.2-confidence-0.2
                                     cxn-decf-0.2-confidence-0.3
                                     cxn-decf-0.3-confidence-0.1
                                     cxn-decf-0.3-confidence-0.3
                                     cxn-decf-0.3-confidence-0.4)))
 :measure-name "lexicon-size"
 :y1-label "Grammar Size"
 :title "Effect of :cxn-decf-score and :learner-speaks-confidence-level"
 :y-min 0 :y-max nil :open t)
