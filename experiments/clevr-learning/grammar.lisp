;;;; grammar.lisp

(in-package :fcg)

(defun detach-punctuation (word)
  "This function will check if the input string (word)
   has a punctuation at the end of it (e.g. it?)
   and return a list of the word + the punctuation mark
   (e.g. '('it' '?')"
  (let ((last-char (char word (1- (length word)))))
    (if (punctuation-p last-char)
      (if (eq last-char #\?)
        (list (subseq word 0 (1- (length word))))
        (list (subseq word 0 (1- (length word)))
              (subseq word (1- (length word)))))
      (list word))))

(defun tokenize (utterance)
  "Split the utterance in words, downcase every word,
   remove the punctuation from the word"
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          append (detach-punctuation (downcase word)))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render (tokenize utterance) :de-render-string-meets-no-punct))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render utterance :de-render-string-meets))

(in-package :clevr-learning)

(defun empty-cxn-set ()
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions-with-type-hierarchy
                     ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:cxn-supplier-mode . :scores)
                                        (:parse-goal-tests :no-applicable-cxns
                                                           :connected-semantic-network
                                                           :no-strings-in-root)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :path-exists) ;; this skips the add-th-links repair
                                        )
                   :visualization-configurations ((:show-constructional-dependencies . nil))))))
    cxn-inventory))

(define-event new-holophrase-cxn (cxn fcg-construction))
(define-event lexicon-changed)

(defun add-holophrase-cxn (grammar form irl-program &key (initial-score 0.5))
  (let* ((cxn-name (gl::make-cxn-name form grammar))
         (form-constraints
          (gl::form-constraints-with-variables
           form (get-configuration grammar :de-render-mode))))
    (multiple-value-bind (cxn-inventory cxn)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?holophrase-unit
                         (syn-cat (grammar-learning::phrase-type grammar-learning::holophrase)))
                        <-
                        (?holophrase-unit
                         (HASH meaning ,irl-program)
                         --
                         (HASH form ,form-constraints)))
                       :cxn-inventory ,grammar
                       :attributes (:score ,initial-score
                                    ;:form ,form
                                    ;:meaning ,(id chunk)
                                    :cxn-type holophrase)))
      (declare (ignorable cxn-inventory))
      (notify new-holophrase-cxn cxn)
      (notify lexicon-changed)
      cxn)))

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "decrease the score of the cxn.
   remove it when it reaches 0"
  (decf (attr-val cxn :score) delta)
  (when (<= (attr-val cxn :score) lower-bound)
    (if remove-on-lower-bound
      (progn (notify lexicon-changed)
        (with-disabled-monitor-notifications
          (delete-cxn cxn (grammar agent))))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun get-form-competitors (agent cxns)
  "Get cxns with the same meaning as cxn"
  (let ((all-cxns-with-meaning
         (remove-duplicates
          (loop for cxn in cxns
                for cxn-meaning = (extract-meaning-predicates cxn)
                append (find-all cxn-meaning
                                 (constructions-list (grammar agent))
                                 :key #'extract-meaning-predicates)))))
    (loop for cxn in cxns
          do (delete cxn all-cxns-with-meaning))
    all-cxns-with-meaning))

(defun get-meaning-competitors (agent cxns)
  "Get cxns with the same form as cxn"
  (let ((all-cxns-with-form
         (remove-duplicates
          (loop for cxn in cxns
                for cxn-form = (list-of-strings->string
                                (render (extract-form-predicates cxn)
                                        (get-configuration (grammar agent) :render-mode)))
                append (find-all cxn-form
                                 (constructions-list (grammar agent))
                                 :key #'(lambda (other-cxn)
                                          (list-of-strings->string
                                           (render (extract-form-predicates other-cxn)
                                                   (get-configuration (grammar agent) :render-mode))))
                                 :test #'string=)))))
    (loop for cxn in cxns
          do (setf all-cxns-with-form
                   (remove cxn all-cxns-with-form)))
    all-cxns-with-form))
  