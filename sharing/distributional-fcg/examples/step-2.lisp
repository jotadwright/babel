(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)

;(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")

(setf *ontonotes-annotations-storage-file*
      (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                      :name "ontonotes-annotations"
                                      :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                       *babel-corpora*))

(setf *ewt-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                                                       :name "ewt-annotations"
                                                                       :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                        *babel-corpora*))



  (load-propbank-annotations 'ewt :ignore-stored-data nil)
  (load-propbank-annotations 'ontonotes :ignore-stored-data nil)

; *ewt-annotations*
; *ontonotes-annotations*


(defparameter *training-configuration*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children :no-applicable-cxns)
    (:max-nr-of-nodes . 10)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2 ;;nr-of-units-matched
     ) ;; edge-weight cannot be used, sometimes there are no neighbours
    ;;Additional heuristics: :prefer-local-bindings :frequency
    
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:sort-cxns-before-application . nil)

    (:node-expansion-mode . :full-expansion)
    (:hash-mode . :hash-lemma)
    
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles)))

(defparameter *propbank-ewt-ontonotes-teach-cxn-inventory* nil)


(learn-distributional-propbank-grammar
 (append (train-split *ewt-annotations*)
         (train-split *ontonotes-annotations*))

 :selected-rolesets '("teach.01")
 :cxn-inventory '*propbank-ewt-ontonotes-teach-cxn-inventory*
 :fcg-configuration *training-configuration*)



;(add-element (make-html *propbank-ewt-ontonotes-teach-cxn-inventory*))


(preprocessing-and-configs *propbank-ewt-ontonotes-teach-cxn-inventory* :step-2)

(cl-store:store *propbank-ewt-ontonotes-teach-cxn-inventory*
                (merge-pathnames (make-pathname   :name "ewt-ontonotes-distributional-teach-grammar-4-april"
                                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                 *babel-corpora*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Comprehend!!!     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(activate-monitor trace-fcg)


;; wrong prediction: children arg1 
(comprehend "he teaches children"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil
            )

;;english is the arg1
(comprehend "he teaches english"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)


;;this one says that students is the arg1!!! not the arg2, heel gek want students zit in het training corpus
(comprehend "he teaches students"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)

;; this one predicts what we would expect (is also in training corpus)
(comprehend "the professor teaches students"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)

;;correct
(comprehend "the professor teaches english"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)




;;ontonotes
;; Jesus arg0 taught FEE the people arg2 in the Temple area every day
;; Mr. Locke arg0 teaches FEE English and comparative literature arg1 at Columbia University .
(comprehend "Jesus taught the people in the Temple area"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)

(comprehend "Mr. Locke teaches English and comparative literature"
            :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
            :timeout nil)


(append (test-split *ewt-annotations*)
        (test-split *ontonotes-annotations*))



(defparameter *teach-sentences*
  (find-sentences-by-role "teach.01" (append (dev-split *ewt-annotations*)
                                             (dev-split *ontonotes-annotations*)
                                             (test-split *ewt-annotations*)
                                             (test-split *ontonotes-annotations*))))


(activate-monitor trace-fcg)

(defparameter *teach-solutions*
  (loop for sentence in *teach-sentences*
        for sentence-string = (sentence-string sentence)
        do (comprehend sentence-string
                       :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
                       :timeout 60)))

(defparameter *teach-solutions-for-paper*
  (loop for sentence in *teach-sentences*
        for sentence-string = (sentence-string sentence)
          when (find sentence-string (list "Again , many people came to him , and Jesus taught them as he always did ."
                                           "One day Jesus was in the Temple area teaching the people ."
                                           "You always teach the truth about God 's way ."
                                           "Also , men from your own group will begin to teach things that are wrong ."
                                           "Jesus taught the people in the Temple area every day .")
                     :test #'string=)
        do (comprehend sentence-string
                       :cxn-inventory *propbank-ewt-ontonotes-teach-cxn-inventory*
                       :timeout 60)))


(defun find-sentences-by-role (role list-of-propbank-sentences)
  (loop for sentence in list-of-propbank-sentences
          for rolesets = (all-rolesets sentence)
        when (find role rolesets :test #'equalp)
          collect sentence))




;; Comprehending "Again , many people came to him , and Jesus taught them as he always did ." ;; dev
;; Comprehending "One day Jesus was in the Temple area teaching the people ." !! ;; dev
;; Comprehending "You always teach the truth about God 's way ." ;; dev
;; Comprehending "Also , men from your own group will begin to teach things that are wrong ." !! ;; dev
;; Comprehending "Jesus taught the people in the Temple area every day ." ?? ;; test

