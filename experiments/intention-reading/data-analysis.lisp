
(ql:quickload :clevr-learning)
(in-package :intention-reading)

(defparameter *experiment-name* "th-link-mixed-mode-smart-tutor")

(defun make-file-path (experiment-name metric)
  (babel-pathname :directory `("experiments" "clevr-learning" "raw-data" ,experiment-name)
                  :name metric :type "lisp"))

(defparameter *communicative-success*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "communicative-success"))
    (read stream)))

(defparameter *grammar-size*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "lexicon-size"))
    (read stream)))

(defparameter *nr-of-lexical-cxns*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "number-of-lexical-cxns"))
    (read stream)))

(defparameter *nr-of-item-based-cxns*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "number-of-item-based-cxns"))
    (read stream)))

;; how many times is the theoretical max of 35 lex cxns reached?
(loop for lex-serie in (first *nr-of-lexical-cxns*)
      count (= (last-elt lex-serie) 35))

;; how many item-based cxns are there in series
;; where the theortical max of 35 lex cxns was achieved?
(loop for lex-serie in (first *nr-of-lexical-cxns*)
      for ib-serie in (first *nr-of-item-based-cxns*)
      if (= (last-elt lex-serie) 35)
      collect (last-elt ib-serie) into a
      else collect (last-elt ib-serie) into b
      finally (return (values (average a) (average b))))

(defparameter *nr-of-holophrase-cxns*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "number-of-holophrase-cxns"))
    (read stream)))

(defparameter *number-of-th-nodes*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "number-of-th-nodes"))
    (read stream)))

(defparameter *number-of-th-edges*
  (with-open-file
      (stream (make-file-path *experiment-name*
                              "number-of-th-edges"))
    (read stream)))

(defun compute-success-at-point (data x)
  (let ((subseries
         (loop for series in (first data)
               collect (subseq series (- x 100) x))))
    (loop for series in subseries
          sum (average series) into averages
          finally
          (return
           (float
            (/ averages (length subseries)))))))

(defun get-size-at-point (data x)
  (let ((points
         (loop for series in (first data)
               collect (nth x series))))
    (list (apply #'min points)
          (average points)
          (apply #'max points))))

(compute-success-at-point *communicative-success* 5000) ;; 78.7
(compute-success-at-point *communicative-success* 10000) ;; 98.9
(compute-success-at-point *communicative-success* 25000) ;; 99.9
(compute-success-at-point *communicative-success* 50000) ;; 99.9
(compute-success-at-point *communicative-success* 100000) ;; 99.9
(compute-success-at-point *communicative-success* 150000) ;; 99.99
(compute-success-at-point *communicative-success* 200000) ;; 100
(compute-success-at-point *communicative-success* 250000) ;; 99.98

(get-size-at-point *grammar-size* 5000) ;; 1048
(get-size-at-point *grammar-size* 25000) ;; 491.9
(get-size-at-point *grammar-size* 249999) ;; 149.3

(get-size-at-point *nr-of-item-based-cxns* 5000) ;; 837.6
(get-size-at-point *nr-of-item-based-cxns* 25000) ;; 299.7
(get-size-at-point *nr-of-item-based-cxns* 249999) ;; 85.2

(get-size-at-point *nr-of-lexical-cxns* 5000) ;; 26.4
(get-size-at-point *nr-of-lexical-cxns* 10000) ;; 32.5
(get-size-at-point *nr-of-lexical-cxns* 249999) ;; 32.8

(get-size-at-point *nr-of-holophrase-cxns* 5000) ;; 184
(get-size-at-point *nr-of-holophrase-cxns* 10000) ;; 181.6
(get-size-at-point *nr-of-holophrase-cxns* 249999) ;; 31.3


(get-size-at-point *number-of-th-nodes* 249999) ;; 213.1
(get-size-at-point *number-of-th-edges* 249999) ;; 1164.9

