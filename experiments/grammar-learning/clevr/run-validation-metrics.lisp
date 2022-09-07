(ql:quickload :grammar-learning)

(in-package :grammar-learning)

(defparameter *learned-cxn-inventory* (cl-store::restore (babel-pathname :directory '("experiments" "grammar-learning" "clevr") :name "cxn-inventory-holo" :type "store")))

(defun load-data (file)
  (with-open-file (stream file)
    (in-package :clevr-world)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect (cons (cdr (assoc :utterance data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal)))))

(defparameter *data* (load-data (merge-pathnames
                                 (make-pathname :directory '(:relative "val")
                                                :name "stage-1" :type "jsonl")
                                 (merge-pathnames
                                  (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                  cl-user:*babel-corpora*))))
(in-package :grammar-learning)

(defmethod comprehend-w-derenderer (utterance &key
                                 (cxn-inventory *fcg-constructions*)
                                 (silent nil))
  (let ((initial-cfs (de-render utterance :de-render-string-meets-no-punct :cxn-inventory cxn-inventory))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterances (listify utterance))
    
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(defun run-validation (cxn-inventory data)
  ;; reset configurations of inventory
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure))
  (set-configuration cxn-inventory :cxn-supplier-mode :hashed-and-scored-routine-cxn-set-only)
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil)
  ;; process all data
  (loop with success-count = 0
        for interaction from 1
        for entry in data
        for utterance = (first entry)
        for gold-std-meaning = (cdr entry)
        for comprehended-meaning = (first (multiple-value-list (comprehend-w-derenderer utterance :cxn-inventory cxn-inventory)))
        for success-p = (irl:equivalent-irl-programs? comprehended-meaning gold-std-meaning)
        do (if success-p
             (progn (incf success-count)
               (format t "~a" "."))
             (format t "~a" "!"))
        (when (= 0 (mod interaction 100))
          (format t " (~a | ~a% overall avg.)~%" interaction (* 100 (float (/ success-count interaction)))))
        finally (return (/ success-count (length data)))))
(* 100 (float (/ 5021 5022)))
;(run-validation *learned-cxn-inventory* *data*)
