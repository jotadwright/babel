(in-package :pattern-finding)

(defun set-up-cxn-inventory-and-repairs ()
  (wi::reset)
  (notify reset-monitors)
  (make-instance 'pattern-finding-experiment
                 :entries `((:meaning-representation . :irl) 
                            (:corpus-files-root . ,(merge-pathnames
                                                    (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                                    cl-user:*babel-corpora*))
                            (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                                 :name "stage-1" :type "jsonl"))
                            (:number-of-samples . 0)
                            (:shuffle-data-p . nil)
                            (:sort-data-p . nil))))