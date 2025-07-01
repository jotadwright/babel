(ql:quickload :geoquery-lsfb-grammar)
(in-package :geoquery-lsfb-grammar)

(defparameter *geoquery-lsfb-data*
  (merge-pathnames
   (make-pathname :directory '(:relative "GeoQuery-LSFB"))
   *babel-corpora*))

(activate-monitor trace-slp)

(defparameter *train-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/train.jsonl"))

(defparameter *test-set*
  (load-geoquery-corpus-jsonl "/Users/liesbetdevos/Projects/Corpora/GeoQuery-LSFB/train-test/test.jsonl"))

(comprehend-list-of-ids '(1 386 485 597 637 754 801 809 810 852))


(comprehend
  (get-example-form 3534 *train-set*)
 :cxn-inventory *geoquery-lsfb*)


(formulate
  (get-example-meaning 3534 *train-set*)
 :cxn-inventory *geoquery-lsfb*)



(test-coverage *train-set* *geoquery-lsfb*)
(test-coverage *test-set* *geoquery-lsfb*)
(length (data  *test-set*))

(pprint (get-example-meaning 3588 *train-set*))
((ANSWER ?E ?A ?F)
 
 (RIVER ?F ?A)
 (TRAVERSE ?F ?A ?B)
 
 ;(STATE ?F ?B)
 ;(NEXT_TO ?F ?B ?C)
 
 ;(STATE ?F ?C)
 ;(LOC ?F ?D ?C)
 ;(CAPITAL ?F ?D)
 
 ;(CONST ?F ?D ?G)
 ;(CITYID ?G ?H ?_)
 ;(MONTGOMERY ?H))

