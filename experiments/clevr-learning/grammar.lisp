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

(defun empty-cxn-set (hide-type-hierarchy cxn-supplier)
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
                   :fcg-configurations ((:cxn-supplier-mode . ,cxn-supplier)
                                        (:parse-order non-holophrase holophrase)
                                        (:parse-goal-tests :no-applicable-cxns
                                                           :connected-semantic-network
                                                           :no-strings-in-root)
                                        (:production-order non-holophrase holophrase)
                                        (:production-goal-tests :no-applicable-cxns
                                                                :connected-structure
                                                                :no-meaning-in-root)
                                        (:max-nr-of-nodes . 1000) ;; !
                                        (:shuffle-cxns-before-application . t)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :neighbours)
                                        (:update-th-links . t))
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-categorial-network . ,(not hide-type-hierarchy)))))))
    cxn-inventory))

(define-event lexicon-changed)

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
          (delete-cxn-and-th-node cxn agent)))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun delete-cxn-and-th-node (cxn agent)
  (let ((lex-class
         (loop for unit in (contributing-part cxn)
               for lex-class = (gl::lex-class-item-based unit)
               when lex-class return lex-class))
        (type-hierarchy (get-type-hierarchy (grammar agent))))
    (delete-cxn cxn (grammar agent))
    (notify lexicon-changed)
    (when lex-class
      (delete-category lex-class type-hierarchy))))

;;;;  COMPETITORS
;;;; -------------

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase))
                                             agent)
  ;; holophrase competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'lexical))
                                             agent)
  ;; lexical competitors have exactly the same form
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-and-render cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-and-render
                    :test #'string=)))
    competitors))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based))
                                             agent)
  ;; meaning competitors for item-based cxns are
  ;; less general item-based cxns and holophrase cxns
  ;; that also work for the current utterance
  (let* ((cxn-name-with-placeholders
         (gl::make-cxn-placeholder-name
          (extract-form-predicates cxn)
          cxn-inventory))
        (de-rendered-utterance
         (fcg::tokenize (utterance agent)))
        (possible-item-based-competitors
         (loop for other-cxn in (constructions-list cxn-inventory)
               when (and (eql (get-cxn-type other-cxn) 'item-based)
                         (< (item-based-number-of-slots other-cxn)
                            (item-based-number-of-slots cxn)))
               collect other-cxn))
        (item-based-competitors
         (loop for comp in possible-item-based-competitors
               for comp-name-with-placeholders =
               (gl::make-cxn-placeholder-name
                (extract-form-predicates comp)
                cxn-inventory)
               when (and (length= cxn-name-with-placeholders
                                  comp-name-with-placeholders)
                         (loop for cxn-elem in cxn-name-with-placeholders
                               for comp-elem in comp-name-with-placeholders
                               for i from 0
                               always (or (string= cxn-elem comp-elem)
                                          (and (string= (subseq cxn-elem 0 1) "?")
                                               (string= (subseq comp-elem 0 1) "?"))
                                          (and (string= (subseq cxn-elem 0 1) "?")
                                               (string= comp-elem (nth i de-rendered-utterance))))))
               collect comp)) 
        (holophrase-competitors
         (loop for other-cxn in (constructions-list cxn-inventory)
               when (and (eql (get-cxn-type other-cxn) 'holophrase)
                         (string= (extract-and-render other-cxn)
                                  (list-of-strings->string
                                   (fcg::tokenize (utterance agent)))))
               collect other-cxn)))
    (append holophrase-competitors item-based-competitors)))

(defun get-meaning-competitors (agent applied-cxns)
  "Get cxns with the same form as cxn"
  (loop for cxn in applied-cxns
        for cxn-type = (get-cxn-type cxn)
        for competitors = (meaning-competitors-for-cxn-type
                           cxn (grammar agent) cxn-type
                           agent)
        append competitors))
