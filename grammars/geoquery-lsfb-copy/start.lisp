(ql:quickload :geoquery-lsfb-grammar-copy)
(in-package :geoquery-lsfb-grammar-copy)

(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

(activate-monitor trace-slp)
(deactivate-monitor trace-slp)

(comprehend
 (get-example-form 779 *train-set*)
 :cxn-inventory *geoquery-lsfb-copy*)

(defparameter *train-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/train.jsonl"))

(defparameter *4500*
  (slp::load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-4500.jsonl"))

(defparameter *test-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/test.jsonl"))

(comprehend-list-of-ids '(1 386 485 597 637 754 801 809 810 852))


(defun test-example (nr)
  (let* ((comprehension-results
          (comprehend-all
           (get-example-form nr *train-set*)
           :cxn-inventory *geoquery-lsfb*))
         (formulation-resulting-forms
          (formulate-all
           (get-example-meaning nr *train-set*)
           :cxn-inventory *geoquery-lsfb*))
         (formulation-comprehended-meanings
          (loop with output = '()
                for result in formulation-resulting-forms
                do (setf output (append output (comprehend-all result :cxn-inventory *geoquery-lsfb*)))
                finally (return output)))
         (comprehension-ok?
          (if (member 0 (loop with output = '()
                              for result in comprehension-results
                              do (if (equivalent-irl-programs? result
                                                               (get-example-meaning nr *train-set*))
                                   (push 1 output)
                                   (push 0 output))
                              finally (return output)))
            "not ok"
            "ok"))
         (formulation-ok?
          (if (member 0 (loop with output = '()
                              for result in formulation-comprehended-meanings
                              do (if (equivalent-irl-programs? result
                                                               (get-example-meaning nr *train-set*))
                                   (push 1 output)
                                   (push 0 output))
                              finally (return output)))
            "not ok"
            "ok")))
    (format t "~%Results for example ~a:~%Comprehension: ~a~%Production: ~a~%" nr comprehension-ok? formulation-ok?)))

(test-example 347)



(multiple-value-bind (result cip)
  (comprehend-all
   (get-example-form 1 *train-set*)
   :cxn-inventory *geoquery-lsfb*)
  (defparameter *result* cip))
  
  (equivalent-irl-programs? *result* 
                            (get-example-meaning 10 *train-set*))

(defparameter *result*
  (formulate-all
  (get-example-meaning 1 *train-set*)
 :cxn-inventory *geoquery-lsfb*))

(test-coverage *train-set* *geoquery-lsfb*)
(test-coverage *test-set* *geoquery-lsfb*)
(length (data  *test-set*))

(pprint (get-example-meaning 1 *train-set*))
(formulate '((const e c f) (countryid f g) (usa g))
           :cxn-inventory *geoquery-lsfb-copy*)
((ANSWER ?C ?A ?D)

 (STATE ?D ?A)
 (LOC ?D ?B ?A)
 
 (LARGEST ?D ?B ?E)
 
 (CITY ?E ?B)
 
 )
(draw-irl-program (get-example-meaning 347 *train-set*) :format "pdf")
