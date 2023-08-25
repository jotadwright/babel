(in-package :pf)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(define-configuration-default-value :corpus-directory
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "CLEVR-pattern-finding-data"))
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :corpus-file
                                    (make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl"))

; mode = learning or evaluation or testing
(define-configuration-default-value :mode :learning)
; how many times the training data is concatenated in random variations
(define-configuration-default-value :number-of-epochs 1) 
(define-configuration-default-value :number-of-samples nil) ; nil = all samples
(define-configuration-default-value :shuffle-data-p t)
(define-configuration-default-value :sort-data-p nil)
(define-configuration-default-value :remove-duplicate-data-p nil)

;; FCG Configurations
(define-configuration-default-value :meaning-representation :irl)  ; :irl or :amr or :geo or ...
(define-configuration-default-value :form-representation :string+meets)  ; :string+meets or :sequences
(define-configuration-default-value :mark-holophrases t)
(define-configuration-default-value :max-number-of-nodes 1000)
(define-configuration-default-value :initial-categorial-link-weight 0.0)
(define-configuration-default-value :category-linking-mode :neighbours)
(define-configuration-default-value :learner-cxn-supplier :hashed-labeled-positive-scores)

;; Learning Operators
(define-configuration-default-value :repairs 
                                    '(add-categorial-links
                                      anti-unify-cipn
                                      anti-unify-cxns                                   
                                      add-cxn))

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.1)
(define-configuration-default-value :evaluation-grammar nil)
(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :remove-cxn-on-lower-bound nil)
(define-configuration-default-value :max-au-cost 10)
(define-configuration-default-value :comprehend-all-n nil)
(define-configuration-default-value :determine-interacting-agents-mode :corpus-learner)

;; Misc
(define-configuration-default-value :categorial-network-export-interval 1000)
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :result-display-interval 100)
(define-configuration-default-value :buffer-size 100)

;; --------------
;; + Experiment +
;; --------------

(defclass pattern-finding-experiment (experiment)
  ((corpus :initarg :corpus :initform nil 
           :accessor corpus :type list
           :documentation "A list of samples for the current challenge level"))
  (:documentation "A grammar learning experiment"))

(defmethod initialize-instance :after ((experiment pattern-finding-experiment) &key)
  (with-configurations ((evaluation-grammar :evaluation-grammar)
                        (mode :mode)) experiment
    ;; set the utterances/gold standard meanings of the experiment
    (unless (eql mode :testing)
      (setf (corpus experiment) (load-corpus experiment)))
    ;; set the population of the experiment
    (setf (population experiment)
          (list (make-pattern-finding-agent experiment)))
    ;; set configurations for evaluation
    (when evaluation-grammar
      (setf (grammar (first (agents experiment))) evaluation-grammar))
    (when (eql mode :evaluation)
      (set-configuration (grammar (first (agents experiment))) :update-categorial-links nil)
      (set-configuration (grammar (first (agents experiment))) :use-meta-layer nil)
      (set-configuration (grammar (first (agents experiment))) :consolidate-repairs nil))))

;; ---------------
;; + Load Corpus +
;; ---------------

(define-event loading-corpus-started)
(define-event loading-corpus-finished)

(defgeneric pre-process-meaning-data (meaning mode))

(defmethod pre-process-meaning-data (meaning (mode (eql :geo)))
  (read-from-string meaning))

(defmethod pre-process-meaning-data (data (mode (eql :irl)))
  (fresh-variables (read-from-string data)))

(defmethod pre-process-meaning-data (meaning (mode (eql :amr)))
  (let ((*package* (find-package "GL-DATA"))
        (parsed-meaning (read-from-string meaning)))
    (if (listp (first parsed-meaning))
      parsed-meaning
      (amr:penman->predicates parsed-meaning))))

(defun load-corpus (experiment)
  (notify loading-corpus-started)
  (with-configurations ((file :corpus-file)
                        (root-dir :corpus-directory)
                        (num-epochs :number-of-epochs)
                        (num-samples :number-of-samples)
                        (shufflep :shuffle-data-p)
                        (sortp :sort-data-p)
                        (remove-duplicates-p :remove-duplicate-data-p)
                        (meaning-representation :meaning-representation)) experiment
    (let* ((challenge-file
            (merge-pathnames file root-dir))
           (file-data
            (with-open-file (stream challenge-file :direction :input)
              (let ((raw-data (stream->list stream)))
                (loop for raw-line in raw-data
                      for line = (decode-json-from-string raw-line)
                      collect (cons (cdr (assoc :utterance line))
                                    (pre-process-meaning-data (cdr (assoc :meaning line))
                                                              meaning-representation)))))))
      (when (and shufflep sortp)
        (error "Cannot shuffle and sort the data"))
      (when remove-duplicates-p
        (setf file-data (remove-duplicates file-data :test #'string= :key #'car)))
      (when shufflep
        (setf file-data (shuffle file-data)))
      (when sortp
        (setf file-data (sort file-data #'< :key #'(lambda (entry) (count #\space (first entry))))))
      (when num-samples
        (setf file-data (subseq file-data 0 num-samples)))
      (when num-epochs
        (setf file-data (loop repeat num-epochs
                              append (if shufflep (shuffle file-data) file-data))))
      (notify loading-corpus-finished)
      file-data)))


(defmethod learner ((experiment pattern-finding-experiment))
  (first (population experiment)))

(defmethod learner ((interaction interaction))
  (first (population (experiment interaction))))


;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------

(defmethod determine-interacting-agents ((experiment pattern-finding-experiment)
                                         interaction (mode (eql :corpus-learner)) &key)
  ;; Tutor is speaker, learner is hearer
  (setf (interacting-agents interaction) (list (learner experiment))
        (discourse-role (learner experiment)) 'hearer)
  (setf (utterance (learner experiment)) nil
        (communicated-successfully (learner experiment)) nil)
  (notify interacting-agents-determined experiment interaction))