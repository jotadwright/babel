(in-package :robot-concept-learning)

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors
                           (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-features-per-form"
                                 "export-utterance-length")))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'mwm-experiment
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :monitors monitors
    :shared-configuration nil
    :configurations strategies
    :output-dir (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names
                                              y-axis y1-max y2-max xlabel y1-label y2-label
                                              captions (open t))
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment-name ,measure-name))
    :average-windows 1000
    :plot-directory `("experiments" "multidimensional-word-meanings" "graphs")
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
                                               captions title start end)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("experiments" "multidimensional-word-meanings" "raw-data" ,experiment-name ,measure-name))
    :average-windows 500
    :captions (if captions captions experiment-names)
    :title title
    :plot-directory '("experiments" "multidimensional-word-meanings" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label)
    :start start :end end)
  (format t "~%Graphs have been created"))



;;;; Utils
(defgeneric category->s-dot-node (category &key)
  (:documentation "How to display a category as an s-dot node"))

(defmethod category->s-dot-node ((category min-max-category) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#990000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (lower-bound category)
                                           (upper-bound category))))))))

(defmethod category->s-dot-node ((category prototype-category) &key green red)
  (let* ((variance (/ (M2 category) (nr-samples category)))
         (stdev (sqrt variance))
         (lower-bound (- (prototype category) (* 2 stdev)))
         (upper-bound (+ (prototype category) (* 2 stdev)))
         (record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           lower-bound
                                           upper-bound)))))))

(defmethod category->s-dot-node ((category prototype-min-max-category) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           (lower-bound category)
                                           (upper-bound category))))))))

(defmethod category->s-dot-node ((category exponential-category) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           (left-sigma category)
                                           (right-sigma category))))))))
  
(defun cxn->s-dot (cxn &optional highlight-green highlight-red)
  (let ((form (attr-val cxn :form))
        (meaning (attr-val cxn :meaning))
        (graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(format nil "~a" form))
                     (s-dot::label ,form)
                     (s-dot::fontcolor "#AA0000"))))
     graph)
    (loop for (category . certainty) in meaning
          for record
          = (category->s-dot-node category
                                  :green (member (attribute category) highlight-green)
                                  :red (member (attribute category) highlight-red))
          when (> certainty 0.0)
          do (push record graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(defun meaning->s-dot (meaning)
  (let ((graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id "root")
                     (s-dot::label ""))))
     graph)
    (loop for (category . certainty) in meaning
          for record = (category->s-dot-node category)
          when (> certainty 0.0)
          do (push record graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,"root")
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(defmethod json-meaning->s-dot-node (meaning (type (eql :min-max-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                          (s-dot::fillcolor "#AAFFAA")))
                                 (red '((s-dot::style "filled")
                                        (s-dot::fillcolor "#990000")))
                                 (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :lower--bound meaning))
                                           (rest (assoc :upper--bound meaning)))))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :prototype-category)) &key green red)
  (let* ((variance (/ (rest (assoc :M2 meaning))
                      (rest (assoc :nr--samples meaning))))
         (stdev (sqrt variance))
         (lower-bound (- (rest (assoc :prototype meaning)) (* 2 stdev)))
         (upper-bound (+ (rest (assoc :prototype meaning)) (* 2 stdev)))
         (record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           lower-bound
                                           upper-bound)))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :prototype-min-max-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           (rest (assoc :lower--bound meaning))
                                           (rest (assoc :upper--bound meaning)))))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :exponential-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           (rest (assoc :left--sigma meaning))
                                           (rest (assoc :right--sigma meaning)))))))))
  
(defun json-cxn->s-dot (cxn &optional highlight-green highlight-red)
  (let ((form (rest (assoc :form cxn)))
        (meaning (rest (assoc :meaning cxn)))
        (graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    ;; form node
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(format nil "~a" form))
                     (s-dot::label ,form)
                     (s-dot::fontcolor "#AA0000"))))
     graph)
    ;; meaning nodes
    (loop with type = (rest (assoc :type cxn))
          for m in meaning
          for record
          = (json-meaning->s-dot-node m (make-kw (upcase type))
                                      :green (member (rest (assoc :attribute m)) highlight-green)
                                      :red (member (rest (assoc :attribute m)) highlight-red))
          when (> (rest (assoc :certainty m)) 0.0)
          do (push record graph))
    ;; edges
    (loop for m in meaning
          when (> (rest (assoc :certainty m)) 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr (rest (assoc :attribute m))))))
                 (s-dot::label ,(format nil "~,2f" (rest (assoc :certainty m))))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))