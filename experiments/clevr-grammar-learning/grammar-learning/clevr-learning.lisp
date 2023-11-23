
(ql:quickload :read-csv)
(ql:quickload :cl-json)
(in-package :cl-user)
(ql:quickload :grammar-learning)
(load (babel-pathname
       :directory '("systems" "fcg" "construction-inventory-processor")
       :name "construction-inventory-processor"
       :type "lisp")) ;; force recompilation
(in-package :cgl)

(deactivate-all-monitors)
(activate-monitor trace-grammar-learning-in-web-interface)
(activate-monitor trace-grammar-learning-verbose)
(activate-monitor trace-grammar-learning-categories)
;(deactivate-monitor trace-grammar-learning-verbose)
;(deactivate-monitor trace-grammar-learning-categories)
;; start the web monitor
;;(activate-monitor trace-fcg)
;; (deactivate-monitor trace-fcg)
;; (activate-monitor trace-fcg-search-process)
;; (deactivate-monitor trace-fcg-search-process)



(defun read-stage-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect (list (cdr (assoc :len data)) (cdr (assoc :question data)) (cdr (assoc :meaning data))))))

(defun pre-process-utterance (utterance)
  "remove punctuation and make lowercase"
   (cl-change-case:no-case utterance))

(defun update-cxn-scores (node)
  (loop for cxn in  (applied-constructions node)
        for cxn-score = (attr-val cxn :score)
        do (setf (attr-val cxn :score) (+ cxn-score 1))))

(defun learn (data &key (num-observations 100) (num-series 1))
  (notify start-experiment)
  (loop for series from 1 to num-series
        do
        (notify start-series series)
        (let* ((cxn-set (eval '(def-fcg-constructions-with-type-hierarchy grammar-learning
                                 :feature-types ((args set)
                                                 (form set-of-predicates)
                                                 (meaning set-of-predicates)
                                                 (subunits set)
                                                 (footprints set))
                                 :fcg-configurations ((:parse-goal-tests :non-gold-standard-meaning)
                                                      (:production-goal-tests :non-gold-standard-utterance)
                                                      (:de-render-mode . :de-render-string-meets)
                                                      (:render-mode . :generate-and-test)
                                                      (:construction-supplier-mode . :ordered-by-label-and-score)
                                                      (:consolidate-repairs . t)
                                                      (:update-th-links . t)
                                ;(:th-connected-mode . :neighbours))
                                                      (:th-connected-mode . :path-exists))
                                 :diagnostics (diagnose-non-gold-standard-meaning diagnose-non-gold-standard-utterance)
                                 :repairs (add-th-links
                                           item-based->lexical
                                           holophrase->item-based+lexical+lexical--substitution
                                           holophrase->item-based+lexical--addition
                                           holophrase->item-based+lexical+holophrase--deletion
                                           repair-lexical->item-based-cxn
                                           nothing->holophrase))))
               (observation-count 0)
               (coverage-count 0)
               (coverage-window 100)
               (error-count 0))
          (loop for observation in (subseq data 0 num-observations)
                for th-flag = (set-data (blackboard *fcg-constructions*) :add-th-links-repair-failed nil)
                for pre-processed-utterance = (pre-process-utterance (second observation))
                for pre-processed-meaning = (remove-duplicates (read-from-string (third observation)) :test #'equal)
                do (incf observation-count)
                (notify show-observation-start observation-count pre-processed-utterance  pre-processed-meaning)
                (let* ((node (second (multiple-value-list                                                
                                      (comprehend
                                       pre-processed-utterance
                                       :gold-standard-meaning pre-processed-meaning))))
                       (repair (third (statuses node)))
                       (car-status (second (statuses node))))
                  
                  (when node
                    (when (or (equal car-status 'CXN-APPLIED) (equal repair 'ADD-TH-LINKS))
                      (incf coverage-count)
                      (update-cxn-scores node))
                    (when (equal car-status 'FCG::DIAGNOSTIC-TRIGGERED)
                      (incf error-count))
                    (notify show-observation-result observation-count pre-processed-utterance pre-processed-meaning car-status repair)
                    (notify show-categories)
                    (when (= (rem observation-count coverage-window) 0)
                      (notify show-progress observation-count coverage-count coverage-window num-observations)
                      (setf coverage-count 0)))))))
  (notify show-results))

(defun run-stage-1 (&key (series 1) (num-observations 1000))
  (let*
      ((clevr-data (read-stage-file (babel-pathname :directory '("systems" "grammar-learning" "storage") :name "stage-1-questions-sorted" :type "txt")))
       ;(observations (shuffle clevr-data))
       (observations clevr-data)
       (clevr-results (learn observations :num-observations num-observations :num-series series)))
    (cl-store:store clevr-results (babel-pathname :directory '("systems" "grammar-learning" "storage") :name "cluster-learning-results" :type "store"))))


;(run-stage-1 :series 1 :num-observations 3000)


(with-open-file (stream "/Users/u0077062/Projects/graph-visualisations/examples/new-th-3k.dot" :direction :output 
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (s-dot:s-dot->dot stream (graph-utils:type-hierarchy->s-dot (graph-utils::graph (categorial-network *fcg-constructions*)))))



