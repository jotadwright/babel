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
    the punctuation from the word"
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          append (detach-punctuation (downcase word)))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render (tokenize utterance) :de-render-string-meets-no-punct))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render utterance :de-render-string-meets))

(in-package :clevr-grammar-learning)

(defun empty-cxn-set (hide-type-hierarchy cxn-supplier th-connected-mode)
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions-with-type-hierarchy
                     ,grammar-name
                   :cxn-inventory ,grammar-name
                   :hashed t
                   :feature-types ((args set)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:cxn-supplier-mode . ,cxn-supplier)
                                        (:parse-goal-tests :non-gold-standard-meaning)
                                        (:production-goal-tests :non-gold-standard-utterance)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:render-mode . :generate-and-test)
                                        (:th-connected-mode . ,th-connected-mode)
                                        (:update-th-links . t)
                                        (:consolidate-repairs . t)
                                        (:hash-mode . :hash-string-meaning-lex-id))
                   :diagnostics (gl::diagnose-non-gold-standard-meaning gl::diagnose-non-gold-standard-utterance)
                   :repairs (gl::add-th-links
                             gl::item-based->lexical
                             gl::holophrase->item-based+lexical+lexical--substitution
                             gl::holophrase->item-based+lexical--addition
                             gl::holophrase->item-based+lexical+holophrase--deletion
                             gl::repair-lexical->item-based-cxn
                             gl::nothing->holophrase)
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
    (if (get-configuration (experiment agent) :remove-cxn-on-lower-bound) 
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
                                             (cxn-inventory hashed-fcg-construction-set)
                                             (cxn-type (eql 'gl::item-based))
                                             agent utterance)
  ;; meaning competitors for item-based cxns are
  ;; less general item-based cxns and holophrase cxns
  ;; that also work for the current utterance
  (let* ((cxn-name-with-placeholders
         (gl::make-cxn-placeholder-name
          (extract-form-predicates cxn)
          cxn-inventory))
        (de-rendered-utterance
         (fcg::tokenize utterance))
        (possible-item-based-competitors
         (loop for other-cxn in (constructions-list cxn-inventory)
               when (and (eql (get-cxn-type other-cxn) 'gl::item-based)
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
               when (and (eql (get-cxn-type other-cxn) 'gl::holophrase)
                         (string= (extract-and-render other-cxn)
                                  (list-of-strings->string
                                   (fcg::tokenize utterance))))
               collect other-cxn)))
    (append holophrase-competitors item-based-competitors)))

(defun get-meaning-competitors (agent applied-cxns utterance)
  "Get cxns with the same form as cxn"
  (loop for cxn in applied-cxns
        for cxn-type = (get-cxn-type cxn)
        for competitors = (when (eql cxn-type 'gl::item-based)
                            (meaning-competitors-for-cxn-type
                             cxn (grammar agent) cxn-type
                             agent utterance))
        append competitors))
