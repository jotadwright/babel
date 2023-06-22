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



(in-package :cgl)

(defun default-clevr-grammar ()
  (let ((clevr-grammar (copy-object *CLEVR*)))
    (set-configurations clevr-grammar
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    (set-configurations (processing-cxn-inventory clevr-grammar)
                        '((:cxn-supplier-mode . :ordered-by-label-hashed)
                          (:priority-mode . :nr-of-applied-cxns)
                          (:parse-order hashed nom cxn)
                          (:production-order hashed-lex nom cxn hashed-morph)
                          (:max-nr-of-nodes . 10000))
                        :replace t)
    clevr-grammar))

(defun empty-cxn-set (hide-type-hierarchy cxn-supplier)
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (hashingp (member cxn-supplier
                           '(:hashed-and-scored
                             :hashed-ordered-by-label
                             :hashed-scored-labeled
                             :hashed-simple-queue)))
         (cxn-inventory
          (eval `(def-fcg-constructions
                     ,grammar-name
                   :cxn-inventory ,grammar-name
                   :hashed ,hashingp
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:cxn-supplier-mode . ,cxn-supplier)
                                        (:parse-goal-tests :no-applicable-cxns
                                                           :connected-semantic-network
                                                           :no-strings-in-root
                                                           :correct-interpretation)
                                        (:production-goal-tests :no-applicable-cxns
                                                                :connected-structure
                                                                :no-meaning-in-root)
                                        (:consolidate-repairs . t)
                                        (:max-nr-of-nodes . 1000)
                                        (:shuffle-cxns-before-application . t)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :neighbours)
                                        (:update-th-links . t)
                                        (:hash-mode . :hash-string-meaning-lex-id)
                                        (:initial-categorial-link-weight . 0.1))
                   :diagnostics (diagnose-failed-interpretation
                                 diagnose-partial-utterance
                                 diagnose-unknown-utterance
                                 diagnose-partial-meaning)
                   :repairs (add-th-links-formulation
                             add-th-links
                             item-based->lexical
                             holophrase->item-based--substitution
                             holophrase->item-based--addition
                             holophrase->item-based--deletion
                             lexical->item-based
                             add-holophrase)
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-categorial-network . ,(not hide-type-hierarchy))
                                                  (:hide-attributes . t))))))
    cxn-inventory))

(define-event lexicon-changed)

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn
                      &key (delta 0.1)
                      (lower-bound 0.0)
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
  "Delete the cxn from the cxn inventory
   and remove ALL associated categories
   from the categorial network."
  (let ((lex-classes
         (loop for unit in (contributing-part cxn)
               for lex-class = (lex-class-item-based unit)
               when lex-class collect lex-class))
        (type-hierarchy (categorial-network (grammar agent))))
    (when lex-classes
      (remove-categories lex-classes type-hierarchy))
    (delete-cxn cxn (grammar agent))    
    (notify lexicon-changed)))

;;;;  COMPETITORS
;;;; -------------

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase))
                                             agent utterance)
  (declare (ignorable utterance))
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
                                             agent utterance)
  (declare (ignorable utterance))
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

(defun placeholderp (str)
  (eql (char str 0) #\?))

(defun competitorp (cxn-name-with-placeholders
                    other-name-with-placeholders
                    de-rendered-utterance)
  ;; also need to check links in the categorial network
  ;; for more abstract item-based cxns??
  (loop for cxn-elem in cxn-name-with-placeholders
        for other-elem in other-name-with-placeholders
        for i from 0
        for nth-word = (nth i de-rendered-utterance)
        always (or ;; same word
                   (string= cxn-elem other-elem)
                   ;; both placeholders
                   (and (placeholderp cxn-elem)
                        (placeholderp other-elem))
                   ;; less abstract
                   (and (placeholderp cxn-elem)
                        (string= other-elem nth-word))
                   ;; more abstract
                   (and (placeholderp other-elem)
                        (stringp cxn-elem)))))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based))
                                             agent utterance)
  ;; meaning competitors for item-based cxns are
  ;; less or more general item-based cxns
  ;; that also work for the current utterance
  ;; and holophrase cxns
  (let* ((cxn-name-with-placeholders
          (make-cxn-placeholder-name
           (extract-form-predicates cxn)
           cxn-inventory))
         (de-rendered-utterance
          (fcg::tokenize utterance))
         (possible-item-based-competitors
          (find-all 'item-based
                    (constructions-list cxn-inventory)
                    :key #'get-cxn-type))
         (item-based-competitors
          (loop for comp in possible-item-based-competitors
                for comp-name-with-placeholders =
                (make-cxn-placeholder-name
                 (extract-form-predicates comp)
                 cxn-inventory)
                when (and (length= cxn-name-with-placeholders
                                   comp-name-with-placeholders)
                          (competitorp cxn-name-with-placeholders
                                       comp-name-with-placeholders
                                       de-rendered-utterance))
                collect comp))
         (holophrase-competitors
          (loop for other-cxn in (constructions-list cxn-inventory)
                when (and (eql (get-cxn-type other-cxn) 'holophrase)
                          (string= (extract-and-render other-cxn)
                                   (list-of-strings->string
                                    (fcg::tokenize utterance))))
                collect other-cxn)))
    (remove cxn (append holophrase-competitors item-based-competitors))))

(defun get-meaning-competitors (agent applied-cxns utterance)
  "Get cxns with the same form as cxn"
  (loop for cxn in applied-cxns
        for cxn-type = (get-cxn-type cxn)
        for competitors = (meaning-competitors-for-cxn-type
                           cxn (grammar agent) cxn-type
                           agent utterance)
        append competitors))

(defun get-form-competitors (agent applied-cxns irl-program)
  "Get cxns that can process the same irl-program"
  (multiple-value-bind (utterances cipns)
      (formulate-all irl-program :cxn-inventory (grammar agent))
    (declare (ignorable utterances))
    (when (> (length cipns) 1)
      (loop for cipn in cipns
            append (original-applied-constructions cipn)
            into other-applied-cxns
            finally
            (return
             (remove-duplicates
              (set-difference other-applied-cxns applied-cxns)))))))


