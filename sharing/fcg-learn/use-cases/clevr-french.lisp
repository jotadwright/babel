(in-package :fcg)

;;(ql:quickload :fcg-learn)



;;#############################################
;; Loading training data for CLEVRançais
;;#############################################

(defparameter *clevr-french-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "clevr-french" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defun load-and-sort-observations (challenge-file &key (sort-p t) )
  ""
  (with-open-file (stream challenge-file)
    (sort
     (loop for line = (read-line stream nil)
           for data = (when line (cl-json:decode-json-from-string line))
           while data
           collect (cons (cdr (assoc :utterance data))
                         (fresh-variables (read-from-string (cdr (assoc :meaning data))))))
     #'< :key #'(lambda (x) (count #\space (first x))))))


(defparameter *sorted-observations-train* (load-and-sort-observations *clevr-french-stage-1-train*))

(defun baseline-grammar (observations)
  (loop with cxn-inventory = (make-sandbox-grammar-cxns)
        for observation in observations
        do (if (constructions cxn-inventory)
             (loop for cxn in (constructions cxn-inventory)
                   do (induce-cxns observation cxn :cxn-inventory cxn-inventory))
             (induce-cxns observation nil :cxn-inventory cxn-inventory))
        finally (return cxn-inventory)))

(setf *first-grammar* (baseline-grammar (subseq *sorted-observations-train* 0 3)))

(activate-monitor trace-fcg)
(comprehend (first (nth 0 *sorted-observations-train*)) :cxn-inventory *first-grammar*)




;;#############################################
;; Testing
;;#############################################

(defparameter *clevr-french-stage-1-test*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "clevr-french" "val")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defparameter *sorted-observations-test* (load-and-sort-observations *clevr-french-stage-1-test*))
(comprehend (first (random-elt *sorted-observations-test*)) :cxn-inventory *holophrase-grammar*)



