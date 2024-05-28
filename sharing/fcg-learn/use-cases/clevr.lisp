(in-package :fcg)

;; (ql:quickload :fcg-learn)
;; (deactivate-all-monitors)
(activate-monitor trace-fcg-learning)
;(activate-monitor trace-fcg)



(def-fcg-constructions empty-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs repair-learn-holophrastic-cxn repair-through-anti-unification)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :lateral-inhibition-avg-entenchment-score)
                       (:li-reward . 0.5)
                       (:li-punishement . 0.5)
                       (:best-solution-mode . :highest-average-entrenchment-score)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))


;;################################
;; Loading training data for CLEVR
;;################################

(defparameter *clevr-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))


(defclass corpus-processor ()
  ((counter :accessor counter :initform 0 :type number)
   (current-speech-act :accessor current-speech-act :initform nil)
   (corpus :accessor corpus :initarg :corpus))
  (:documentation "Class for corpus processor"))

(defmethod load-corpus ((path pathname) &key (sort-p t))

  (let ((speech-acts (with-open-file (stream path)
                       (loop for line = (read-line stream nil)
                             for data = (when line (cl-json:decode-json-from-string line))
                            
                             while data
                             collect (make-instance 'speech-act
                                                   :form (cdr (assoc :utterance data))
                                                   :meaning 
                                                   (pn::instantiate-predicate-network (read-from-string (cdr (assoc :meaning data)))))))))

    (when sort-p
      (sort speech-acts #'< :key #'(lambda (speech-act) (count #\space (form speech-act)))))

    (make-instance 'corpus-processor
                   :corpus (make-array (length speech-acts) :initial-contents speech-acts))))
                   

(defmethod next-speech-act ((cp corpus-processor))

  (setf (current-speech-act cp)
        (aref (corpus cp) (counter cp)))

  (incf (counter cp))

  (values (current-speech-act cp) (counter cp)))

(defmethod nth-speech-act ((cp corpus-processor) (n number))

  (aref (corpus cp) (- n 1)))

 
(defmethod reset-cp ((cp corpus-processor))
  
  (setf (counter cp) 0)
  (setf (current-speech-act cp) nil))
  

(defparameter *clevr-processor* (load-corpus *clevr-stage-1-train*))

(next-speech-act *clevr-processor*)

(nth-speech-act *clevr-processor* 45000)



(defparameter *clevr-grammar* (make-empty-cxn-inventory-cxns))
(reset-cp *clevr-processor*)

(comprehend (current-speech-act *clevr-processor*) :cxn-inventory *clevr-grammar* :learn nil :align nil :consolidate nil)












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



