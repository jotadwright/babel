(ql:quickload :clg)
(in-package :clg)

(defun create-graph-for-single-strategy (experiment-group
                                         experiment-name
                                         experiment-run-name
                                         measure-names
                                         &rest evo-plot-keyword-args)
  ;; take some arguments, but pass along the rest to raw-files->evo-plot
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-group measure-names)
  (let* ((raw-file-paths (loop for measure-name in measure-names
                               collect `("experiments"
                                         "concept-grammar-learning"
                                         "logging"
                                         ,experiment-group
                                         "val"
                                         ,experiment-name
                                         ,experiment-run-name
                                         ,measure-name)))
         (default-plot-file-name
          (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                  raw-file-paths :key #'(lambda (path) (first (last path)))))
         (plot-file-name
          (when (find :plot-file-name evo-plot-keyword-args)
            (nth (1+ (position :plot-file-name evo-plot-keyword-args)) evo-plot-keyword-args))))
    (apply #'raw-files->evo-plot
           (append `(:raw-file-paths ,raw-file-paths
                     :plot-directory ("experiments" "concept-grammar-learning" "logging" ,experiment-group)
                     :plot-file-name ,(if plot-file-name plot-file-name default-plot-file-name))
                   evo-plot-keyword-args)))
  (format t "~%Graphs have been created."))

(create-graph-for-single-strategy "clevr"
                                  "clevr-extracted"
                                  "2025-05-20_9h53m0s-seed42-91542"
                                  '("communicative-success"
                                    "number-of-holophrase-cxns"
                                    "number-of-item-based-cxns"
                                    "number-of-lexical-cxns")
                                  :average-windows '(1000 1 1 1)
                                  :use-y-axis '(1 2 2 2)
                                  :y1-min 0 :y1-max 1
                                  :y2-min 0 :y1-max nil
                                  :x-label "Number of Games"
                                  :y1-label "Communicative Success"
                                  :y2-label "Number of Constructions"
                                  :captions '("communicative success" "holophrase" "item-based" "lexical")
                                  ;:end 10000 
                                  :open nil)


(loop for measure-name in (list "a" "b")
      collect `("experiments"
                "concept-grammar-learning"
                "logging"
                ,measure-name                 
                "val"
                "clevr-simulated"
                "2025-05-20_0h53m50s-seed42-70593"
                ))