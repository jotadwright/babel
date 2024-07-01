(in-package :fcg)

;;(ql:quickload :fcg-learn)



;;#############################################
;; Loading training data for AMR 3.0
;;#############################################

(defparameter *amr-3.0*
  (merge-pathnames (make-pathname :directory '(:relative "amr-corpora" "amr_annotation_3.0" "pre-processed")
                                  :name "amr3" 
                                  :type "json")
                   cl-user:*babel-corpora*))

(defun load-and-sort-observations (challenge-file &key (sort-p t) )
  ""
  (with-open-file (stream challenge-file)
    (sort
     (loop for line = (read-line stream nil)
           for data = (when line (cl-json:decode-json-from-string line))
           while data
           collect (cons (cdr (assoc :utterance data))
                         (fresh-variables (amr:penman->predicates (read-from-string (cdr (assoc :meaning data))) :variablify? t))))
     #'< :key #'(lambda (x) (count #\space (first x))))))


(defparameter *sorted-observations-train* (load-and-sort-observations *amr-3.0*))

(defun holophrase-grammar (observations)
  (loop with cxn-inventory = (make-sandbox-grammar-cxns)
        for observation in observations
        unless (or (search "http" (first observation))
                   (search "[" (first observation)))
        do (induce-cxns observation nil :cxn-inventory cxn-inventory)
        finally (return cxn-inventory)))

(defparameter *first-grammar* (holophrase-grammar (subseq *sorted-observations-train* 0 10000)))

(activate-monitor trace-fcg)
(loop for i from 8000 to 8020
        do
        (comprehend (first (nth i *sorted-observations-train*)) :cxn-inventory *first-grammar*))
