;;;; utils.lisp

(in-package :clevr-learning)

(defun extract-meanings-from-cipn (cipn)
  (extract-meanings
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun extract-forms-from-cipn (cipn)
  (extract-forms
   (left-pole-structure
    (car-resulting-cfs
     (cipn-car cipn)))))

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (1- (length (contributing-part cxn)))))

(defun get-strings-from-root (node)
  (gl::form-predicates-with-variables
   (extract-string
    (get-root
     (if (find 'fcg::second-merge-failed (fcg::statuses node))
       (car-first-merge-structure (cipn-car node))
       (left-pole-structure
        (car-resulting-cfs
         (cipn-car node))))))))

(defun set-cxn-last-used (agent cxn)
  (let ((current-interaction-nr
         (interaction-number
          (current-interaction
           (experiment agent)))))
    (setf (attr-val cxn :last-used) current-interaction-nr)))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))

(defun find-equivalent-item-based-cxn (number-of-slots meaning-predicates form-predicates grammar)
  "Look through the grammar for an item-based cxn that is equivalent
   based on the number of slots, the meaning and the form"
  (let ((equivalent
         (find-all-if #'(lambda (cxn)
                          (and ;; cxn has to be item-based
                               (eql (get-cxn-type cxn) 'item-based)
                               ;; with the same number of slots
                               (= (item-based-number-of-slots cxn)
                                  number-of-slots)
                               ;; an equivalent meaning
                               (equivalent-irl-programs?
                                (extract-meaning-predicates cxn)
                                meaning-predicates)
                               ;; and the same form
                               (string=
                                (list-of-strings->string
                                 (gl::make-cxn-placeholder-name
                                  form-predicates grammar))
                                (list-of-strings->string
                                 (gl::make-cxn-placeholder-name
                                  (extract-form-predicates cxn)
                                  grammar)))))
                      (constructions-list grammar))))
    (when equivalent
      (random-elt equivalent))))

(defun find-equivalent-lexical-cxn (meaning-predicates form-predicates grammar)
  "Look through the grammar for a lexical cxn that is equivalent
   based on the meaning and the form"
  (let ((equivalent
         (find-all-if #'(lambda (cxn)
                          (and ;; cxn has to be lexical
                               (eql (get-cxn-type cxn) 'lexical)
                               ;; with an equivalent meaning
                               (equivalent-irl-programs?
                                (extract-meaning-predicates cxn)
                                meaning-predicates)
                               ;; and the same form
                               (string=
                                (third (first form-predicates))
                                (third (first (extract-form-predicates cxn))))))
                      (constructions-list grammar))))
    (when equivalent
      (random-elt equivalent))))

(defun equal-item-based-cxns-p (cxn-a cxn-b)
  (and
   ;; with the same number of slots
   (= (item-based-number-of-slots cxn-a)
      (item-based-number-of-slots cxn-b))
   ;; an equivalent meaning
   (equivalent-irl-programs?
    (extract-meaning-predicates cxn-a)
    (extract-meaning-predicates cxn-b))
   ;; and the same form
   (string=
    (list-of-strings->string
     (gl::make-cxn-placeholder-name
      (extract-form-predicates cxn-a)
      (cxn-inventory cxn-a)))
    (list-of-strings->string
     (gl::make-cxn-placeholder-name
      (extract-form-predicates cxn-b)
      (cxn-inventory cxn-a))))))

(defun get-cxns-of-type (agent type)
  (if (eql type 'all)
    (constructions-list (grammar agent))
    (find-all type (constructions-list (grammar agent))
              :key #'get-cxn-type)))

(defun cxn-score (cxn)
  (attr-val cxn :score))


;;;; UTILS FOR RUNNING GAMES
;;;; -----------------------

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors (get-all-lisp-monitors)))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'clevr-learning-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :named-configurations strategies
    :shared-configuration nil
    :monitors monitors
    :output-dir (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

;;;; UTILS FOR PLOTTING
;;;; ------------------

(defun create-graph-for-single-strategy (&key experiment-name measure-names
                                              y-axis y1-max y2-max xlabel y1-label y2-label
                                              captions open)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 100
    :plot-directory `("experiments" "clevr-learning" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :captions captions
    :use-y-axis y-axis
    :y1-min 0
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :open open)
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name
                                               (y-min 0) (y-max 1) xlabel y1-label y2-label
                                               captions title)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "clevr-learning" "raw-data" ,experiment-name ,measure-name))
    :average-windows 500
    :captions (if captions captions experiment-names)
    :title (if title title "")
    :plot-directory '("experiments" "clevr-learning" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))