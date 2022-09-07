(ql:quickload :grammar-learning)

(in-package :grammar-learning)

(defparameter *learned-cxn-inventory* (cl-store::restore (babel-pathname :directory '("experiments" "grammar-learning" "clevr-french") :name "cxn-inventory-holo" :type "store")))
(defun load-data (file)
  (with-open-file (stream file)
    (in-package :clevr-world)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect (cons (cdr (assoc :utterance data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal)))))

(defparameter *data* (load-data (merge-pathnames
                                 (make-pathname :directory '(:relative "clevr-french" "val")
                                                :name "stage-1" :type "jsonl")
                                 (merge-pathnames
                                  (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                  cl-user:*babel-corpora*))))
(in-package :grammar-learning)


(run-validation *learned-cxn-inventory* *data*)
;(add-element (make-html *learned-cxn-inventory* :sort-by-type-and-score t :routine-only t))

