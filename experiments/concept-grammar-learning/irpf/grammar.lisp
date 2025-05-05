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

(in-package :clg)

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
                                        (:parse-goal-tests
                                         :no-applicable-cxns
                                         :connected-semantic-network
                                         :no-strings-in-root
                                         :correct-interpretation
                                         )
                                        (:production-goal-tests
                                         :no-applicable-cxns
                                         :connected-structure
                                         :no-meaning-in-root)
                                        (:consolidate-repairs . t)
                                        (:max-nr-of-nodes . 1000)
                                        (:shuffle-cxns-before-application . t)
                                        (:construction-inventory-processor-mode . :default)
                                        (:node-expansion-mode . :default)
                                        (:queue-mode . :greedy-best-first)
                                        (:priority-mode . :nr-of-applied-cxns)
                                        (:render-mode . :generate-and-test)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :neighbours)
                                        (:update-th-links . t)
                                        (:hash-mode . :hash-string-meaning-lex-id)
                                        (:initial-categorial-link-weight . 0.1))
                   :diagnostics (diagnose-failed-interpretation
                                 diagnose-partial-utterance
                                 diagnose-unknown-utterance
                                 diagnose-partial-meaning
                                 )
                   :repairs (add-th-links-formulation
                             add-th-links
                             ;item-based->lexical
                             ;holophrase->item-based--substitution
                             ;holophrase->item-based--addition
                             ;holophrase->item-based--deletion
                             lexical->item-based
                             ;add-holophrase
                             )
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-categorial-network . ,(not hide-type-hierarchy))
                                                  (:hide-attributes . t))))))
    cxn-inventory))



