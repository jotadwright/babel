(in-package :geoquery-lsfb-grammar-copy)

(defun same-arguments (pred-1 pred-2)
  (when (and (eql (first pred-1)(first pred-2))
             (eql (second pred-1)(second pred-2))
             (eql (third pred-1)(third pred-2)))
    t))
                  

(defun coincides->simultaneous-predicates (signed-form-predicates)
  (loop with output = '()
        with included-simultaneous-predicates = '()
        for predicate in (slp::predicates signed-form-predicates)
        for during-predicate = `(during ,(second predicate) ,(third predicate))
        do (cond ((and (member (first predicate) '(slp::start-coincides slp::end-coincides slp::during))
                        (not (member during-predicate included-simultaneous-predicates :test #'same-arguments)))
                  (progn (push during-predicate
                               output)
                    (push during-predicate
                          included-simultaneous-predicates)))
                  ((and (member (first predicate) '(slp::start-coincides slp::end-coincides slp::during))
                        (member during-predicate included-simultaneous-predicates :test #'same-arguments))
                   nil)
                  (t
                   (push predicate output)))
        finally (return (make-instance 'slp::signed-form-predicates
                                       :predicates output))))

(defun get-example-form (id data-processor)
  "gets the form of the example with the provided id from the data-processor"
  (loop for sign-act in (slp::data data-processor)
        when (= (slp::id sign-act) id)
          do (return
              (coincides->simultaneous-predicates
              (slp::form sign-act)))))

(defun get-example-meaning (id data-processor)
  "gets the form of the example with the provided id from the data-processor"
  (loop for sign-act in (slp::data data-processor)
        when (= (slp::id sign-act) id)
          do (return
              (slp::meaning sign-act))))

(defun comprehend-list-of-ids (list-of-ids)
  (loop for id in list-of-ids
        do (comprehend
            (get-example-form id *train-set*)
            :cxn-inventory slp::*geoquery-lsfb*)))

(defparameter *geoquery-states*
  (jsonl->list-of-json-alists
   (babel-pathname :directory '("systems" "sign-language-processing""sign-language-processing-requirements")
                   :name "states"
                   :type "jsonl")))

(defparameter *geoquery-states-naming-signs*
  (jsonl->list-of-json-alists
   (babel-pathname :directory '("systems" "sign-language-processing""sign-language-processing-requirements")
                   :name "naming-signs"
                   :type "jsonl")))

(defparameter *geoquery-cities*
  (jsonl->list-of-json-alists
   (babel-pathname :directory '("systems" "sign-language-processing""sign-language-processing-requirements")
                   :name "cities"
                   :type "jsonl")))

(defparameter *geoquery-rivers*
  (jsonl->list-of-json-alists
   (babel-pathname :directory '("systems" "sign-language-processing""sign-language-processing-requirements")
                   :name "rivers"
                   :type "jsonl")))


(defun determine-additional-categories (state-name)
  (let ((additional-categories '()))
    (loop for city in *geoquery-cities*
          for city-name = (replace-spaces (cdr (assoc :name city)) :replacer "_")
          when (string= state-name city-name)
            do (push 'city additional-categories))
    (loop for river in *geoquery-rivers*
          for river-name = (replace-spaces (cdr (assoc :name river)) :replacer "_")
          when (string= state-name river-name)
            do (push 'river additional-categories))
    additional-categories))