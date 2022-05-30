(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; Cleaning a learned PropBank grammar    ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun clean-grammar (grammar dev-corpus &key (destructive t) (nr-of-test-sentences 100) (timeout 10) (cut-off 3000))
  "Clean the grammar for erroneous constructions that are a result of annotation errors."
  (format t "~%>>Grammar size before cleaning: ~a ~%" (size grammar)) 
  (loop with cxn-inventory = (if destructive grammar (copy-object grammar))
        for (cxn . dev/train-ratio) in (sort-cxns-for-outliers cxn-inventory dev-corpus :timeout timeout
                                                               :nr-of-training-sentences (get-data (blackboard grammar) :training-corpus-size)
                                                               :nr-of-test-sentences nr-of-test-sentences)
        if (>= (eval dev/train-ratio) cut-off)
        do (with-disabled-monitor-notifications
             (delete-cxn cxn cxn-inventory :hash-key (attr-val cxn :lemma))
             (delete-cxn cxn (processing-cxn-inventory cxn-inventory) :hash-key (attr-val cxn :lemma)))
        else do (return cxn-inventory)))


(defun sort-cxns-for-outliers (learned-propbank-grammar dev-corpus &key (nr-of-test-sentences 100)
                                                                     (timeout 10)
                                                                     (nr-of-training-sentences nil))
  "Run the learned grammar on a number of sentences of the dev-corpus in order to detect faulty cxns."
  (assert nr-of-training-sentences)
  (let* ((selected-test-sentences (subseq (shuffle dev-corpus) 0 nr-of-test-sentences))
         (test-frequencies-and-nr-of-timeouts
          (multiple-value-list (collect-cxn-frequencies learned-propbank-grammar
                                                        (mapcar #'sentence-string selected-test-sentences)
                                                        :timeout timeout)))
         (cxns-w-score
          (sort
           (loop for cxn in (constructions-list learned-propbank-grammar)
                 for cxn-test-frequency = (gethash (name cxn) (first test-frequencies-and-nr-of-timeouts))
                 when (> cxn-test-frequency 0)
                 collect (cons cxn `(/ ,(float (/ cxn-test-frequency ;;percentage of occurrence in testing
                                                  (- (length selected-test-sentences) (second test-frequencies-and-nr-of-timeouts)))
                                               )
                                       ,(float (/ (attr-val cxn :frequency) ;;percentage of occurrence in training
                                                  nr-of-training-sentences)))))
           #'> :key #'(lambda (cxn-w-score) (abs (eval (cdr cxn-w-score)))))))

    (loop for (cxn . score) in cxns-w-score
          unless (< (abs (eval score)) 0.02)
          do (format t "~a: ~a (~$) ~%" (name cxn) score (abs (eval score))))
    
    cxns-w-score))


(defun collect-cxn-frequencies (hashed-cxn-inventory list-of-sentences &key (timeout 10))
  "Returns a hash table with as keys the cxns of the cxn-inventory and
as value the frequency of every construction in the application of the
grammar on the list-of-sentences"
  (let ((frequency-table ;;initialization
         (loop with freq-table = (make-hash-table)
               for cxn in (constructions-list hashed-cxn-inventory)
               do (setf (gethash (name cxn) freq-table) 0)
               finally (return freq-table)))
        (nr-of-time-outs 0))

    (loop for sentence in list-of-sentences
          for comprehension-result = (multiple-value-list
                                      (comprehend sentence :cxn-inventory hashed-cxn-inventory :silent t :timeout timeout))
          if (eq 'time-out (first comprehension-result))
          do (progn (incf nr-of-time-outs)
                    (format t "x"))
          else do (progn (format t ".")
                         (loop for cxn in (applied-constructions (second comprehension-result))
                               do (incf (gethash (name cxn) frequency-table)))))
          
    (values frequency-table nr-of-time-outs)))


(defun apply-cutoff (grammar &key (cutoff 200) sorted-cxn-list)
  "Delete all constructions that occur N times more frequently in the
development corpus than in the training corpus."
  (loop for (cxn . dev/train-ratio) in (reverse sorted-cxn-list)
        if (>= (eval dev/train-ratio) cutoff)
        do (progn (delete-cxn (name cxn) grammar :key #'name)
                  (delete-cxn (name cxn) (processing-cxn-inventory grammar) :key #'name)
                  (format t "."))))


(defun delete-have-and-be-cxns (grammar)
  "Delete all constructions under the hash keys 'be' and 'have',
together with all constructions for 'be' and 'have' that are stored
under different keys"
  (format t "Nr of cxns before cleaning (FCG-2): ~a ~%" (size grammar))
  (format t "Nr of cxns before cleaning (FCG-1): ~a ~%" (size (processing-cxn-inventory grammar)))
  
  (remhash 'be (constructions-hash-table grammar))
  (remhash 'be (constructions-hash-table (processing-cxn-inventory grammar)))
  (remhash 'have (constructions-hash-table grammar))
  (remhash 'have (constructions-hash-table (processing-cxn-inventory grammar)))
  
  (loop for v being each hash-values of (constructions-hash-table grammar) using (hash-key k)
        for remaining-cxns = (loop for cxn in v
                                   unless (or (search "BE." (subseq (mkstr (name cxn)) 0 3))
                                              (search "HAVE." (mkstr (name cxn))))
                                   collect cxn)
        do (setf (gethash k (constructions-hash-table grammar)) remaining-cxns))

  (loop for v being each hash-values of (constructions-hash-table (processing-cxn-inventory grammar)) using (hash-key k)
        for remaining-cxns = (loop for cxn in v
                                   unless (or (search "BE." (subseq (mkstr (name cxn)) 0 3))
                                              (search "HAVE." (mkstr (name cxn))))
                                   collect cxn)
        do (setf (gethash k (constructions-hash-table grammar)) remaining-cxns))

  (format t "Nr of cxns after cleaning (FCG-2): ~a ~%" (size grammar))
  (format t "Nr of cxns after cleaning (FCG-1): ~a ~%" (size (processing-cxn-inventory grammar)))

  )


;(delete-have-and-be-cxns *restored-grammar-sbcl*)
