(ql:quickload :geoquery-lsfb-grammar)
(in-package :geoquery-lsfb-grammar)

(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

(activate-monitor trace-slp)

(defparameter *train-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/train.jsonl"))

(defparameter *4500*
  (slp::load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-4500.jsonl"))

(defparameter *test-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/test.jsonl"))

(comprehend-list-of-ids '(1 386 485 597 637 754 801 809 810 852))


(defparameter *result*
  (comprehend
  (get-example-form 573 *train-set*)
 :cxn-inventory *geoquery-lsfb*))

(equivalent-irl-programs? *result* 
                          (get-example-meaning 573 *train-set*))

(formulate-all
  (get-example-meaning 573 *train-set*)
 :cxn-inventory *geoquery-lsfb*)

(test-coverage *train-set* *geoquery-lsfb*)
(test-coverage *test-set* *geoquery-lsfb*)
(length (data  *test-set*))

(pprint (get-example-meaning 573 *train-set*))
((ANSWER ?C ?A ?D)
 
 (SMALLEST ?D ?A ?E)
 
 (CITY ?E ?A)
 (LOC ?E ?A ?B)
 
 (CONST ?E ?B ?F)
 (COUNTRYID ?F ?G)
 (USA ?G))
(draw-irl-program (get-example-meaning 2801 *train-set*) :format "pdf")
