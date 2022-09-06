(ql:quickload :grammar-learning)
(in-package :grammar-learning)

(let ((experiment-names
       '(training-10-series)))
  (run-parallel-batch-for-different-configurations 
   :asdf-system "grammar-learning"
   :package "grammar-learning"
   :experiment-class "grammar-learning-experiment"
   :number-of-interactions 47134
   :number-of-series 10
   :monitors (append (get-all-export-monitors)
                     (get-all-lisp-monitors)
                     (get-all-csv-monitors))
   :configurations '(
                     (training-10-series
                      ((:number-of-epochs . 1)
                       ))
                     )
   :shared-configuration `(
                           (:observation-sample-mode . :train)
                           (:meaning-representation . :irl)
                           (:de-render-mode . :de-render-string-meets-no-punct)
                           (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                           (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))
                           (:repairs . (add-categorial-links
                                           item-based->item-based--substitution
                                           item-based->holistic
                                           holistic->item-based--substitution
                                           ;holistic->item-based--addition
                                           ;holistic->item-based--deletion
                                           holistic->item-based
                                           nothing->holistic))
                           )
   :output-dir (babel-pathname :directory '("experiments" "grammar-learning" "clevr" "raw-data"))
   :heap-size 12288))
