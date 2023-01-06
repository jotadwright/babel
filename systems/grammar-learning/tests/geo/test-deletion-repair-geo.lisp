(ql:quickload :grammar-learning) 
(in-package :grammar-learning)

(defun test-deletion-repair-comprehension-geo ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-geo))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "How long is the Mississippi river ?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((ANSWER ?C ?A ?D)
                                             (LEN ?D ?B ?A)
                                             (CONST ?D ?B ?E)
                                             (RIVERID ?E ?F)
                                             (MISSISSIPPI ?F)
                                             (RIVER ?D ?B)))
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "How long is the Mississippi ?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((ANSWER ?C ?A ?D)
                                             (LEN ?D ?B ?A)
                                             (CONST ?D ?B ?E)
                                             (RIVERID ?E ?F)
                                             (MISSISSIPPI ?F))))))
    (comprehend "How long is the Mississippi river ?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((ANSWER ?C ?A ?D)
                                             (LEN ?D ?B ?A)
                                             (CONST ?D ?B ?E)
                                             (RIVERID ?E ?F)
                                             (MISSISSIPPI ?F)
                                             (RIVER ?D ?B)))
    (add-element (make-html cxn-inventory))))





(defun run-deletion-tests ()
  (test-deletion-repair-comprehension-geo)
  
  )



;(run-deletion-tests)

#|
(defvar *fc-1* '((STRING GRAMMAR-LEARNING::?HOW-4255 "how") (STRING GRAMMAR-LEARNING::?LONG-543 "long") (STRING GRAMMAR-LEARNING::?IS-6507 "is") (STRING GRAMMAR-LEARNING::?THE-14222 "the") (STRING GRAMMAR-LEARNING::?MISSISSIPPI-788 "mississippi") (STRING GRAMMAR-LEARNING::?RIVER-1857 "river") (FCG:MEETS GRAMMAR-LEARNING::?HOW-4255 GRAMMAR-LEARNING::?LONG-543) (FCG:MEETS GRAMMAR-LEARNING::?LONG-543 GRAMMAR-LEARNING::?IS-6507) (FCG:MEETS GRAMMAR-LEARNING::?IS-6507 GRAMMAR-LEARNING::?THE-14222) (FCG:MEETS GRAMMAR-LEARNING::?THE-14222 GRAMMAR-LEARNING::?MISSISSIPPI-788) (FCG:MEETS GRAMMAR-LEARNING::?MISSISSIPPI-788 GRAMMAR-LEARNING::?RIVER-1857)))

(defvar *fc-2* '((STRING GRAMMAR-LEARNING::?HOW-4252 "how") (STRING GRAMMAR-LEARNING::?LONG-540 "long") (STRING GRAMMAR-LEARNING::?IS-6504 "is") (STRING GRAMMAR-LEARNING::?THE-14219 "the") (STRING GRAMMAR-LEARNING::?MISSISSIPPI-785 "mississippi") (FCG:MEETS GRAMMAR-LEARNING::?HOW-4252 GRAMMAR-LEARNING::?LONG-540) (FCG:MEETS GRAMMAR-LEARNING::?LONG-540 GRAMMAR-LEARNING::?IS-6504) (FCG:MEETS GRAMMAR-LEARNING::?IS-6504 GRAMMAR-LEARNING::?THE-14219) (FCG:MEETS GRAMMAR-LEARNING::?THE-14219 GRAMMAR-LEARNING::?MISSISSIPPI-785)))

(diff-form-constraints *fc-1* *fc-2*) ; long short

summary: long short
non-overlapping-form-observation: river
non-overlapping-form-cxn: nil
overlapping-form-observation: obs+X
overlapping-form-cxn: obs

(diff-form-constraints *fc-2* *fc-1*)
expected: short long
non-overlapping-form-observation: nil
non-overlapping-form-cxn: river
overlapping-form-observation: obs
overlapping-form-cxn: obs+X


|#