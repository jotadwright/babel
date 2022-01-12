(in-package :emergent-rl)

(defun create-graph-for-single-strategy (&key experiment-name measure-names y-axis y1-max y2-max xlabel y1-label y2-label x-max plot-file-name (captions nil) (avg-window 100))
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "emergent-rl" "raw-data" ,experiment-name "monitors" ,measure-name))
    :average-windows avg-window
    :plot-directory `("experiments" "emergent-rl" "graphs" ,experiment-name)
    :error-bars '(:stdev)
    :error-bar-modes '(:filled)
    :use-y-axis y-axis
    :y1-min 0
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :end x-max
    :x-label (if xlabel xlabel "number of games")
    :y1-label y1-label
    :y2-label y2-label
    :open nil
    :plot-file-name plot-file-name
    :captions captions
    :fsize 12
    )
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key top-name experiment-names measure-name (avg-window 100) (y-min 0) (y-max 1) x-max xlabel y1-label y2-label plot-file-name (captions nil) (key-location "below"))
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "emergent-rl" "raw-data" ,top-name ,experiment-name "monitors" ,measure-name))
    :average-windows avg-window
    :plot-directory `("experiments" "emergent-rl" "graphs" ,top-name)
    :error-bars '(:stdev)
    :error-bar-modes '(:filled)
    :y1-min y-min
    :y1-max y-max
    :end x-max
    :x-label (if xlabel xlabel "number of games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open nil
    :plot-file-name plot-file-name
    :key-location key-location
    :captions (if captions captions experiment-names)
    :fsize 12
    )
  (format t "~%Graphs have been created"))


;; creates meaning-form competition graphs
(define-monitor record-meaning-form-competition
                :documentation "Record meaning-form competition across a single agent"
                :class 'alist-recorder
                :average-windows 1)

(define-monitor plot-meaning-form-competition
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-meaning-form-competition
    :average-windows 1
    :draw-y-1-grid t
    :fsize 12
    :y-label "forms for object 3"
    :x-label "number of games"
    :file-name (babel-pathname
                :directory '("experiments" "emergent-rl" "raw-data" "comp" "int" "monitors")
                :name "meaning-form-competition"
                :type "pdf")
    :graphic-type "pdf")

;; to create form competition graphs for a single agent
(defun create-fm-competition-graph (&key experiment-dir measure-name)
  ;; this function allows to plot form-competition graphs for a single agent during an experiment
  (format t "~%Creating graph for ~a with measure ~a" experiment-dir measure-name)
  (activate-monitor plot-meaning-form-competition)
  (notify reset-monitors)
  (with-open-file (stream (babel-pathname
                           :name measure-name
                           :type "lisp"
                           :directory experiment-dir)
                           :direction :input)
    (let* ((data (car (read stream nil nil nil)))
           (forms (mapcar #'car data))
           (lists-of-scores (apply #'mapcar #'list (mapcar #'cadr data))))
      (loop for scores in lists-of-scores
            for idx from 1
            do (notify interaction-started t t idx)
            (loop for form in forms
                  for score in scores
                  when score
                  do (set-value-for-symbol record-meaning-form-competition form score))
            do (notify interaction-finished t t idx))))
  (notify series-finished 1)
  (notify batch-finished "")
  (deactivate-monitor plot-meaning-form-competition))
