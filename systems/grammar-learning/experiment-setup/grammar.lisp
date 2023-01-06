;;;; grammar.lisp

(in-package :fcg)



(defun tokenize (utterance)
  "Remove the punctuation from the sentence. Split the utterance in words, downcase every word"
  (let ((words (split (remove-spurious-spaces (remove-punctuation utterance)) #\space)))
    (mapcar #'downcase words)))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (de-render (tokenize utterance) :de-render-string-meets-no-punct))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-no-punct))
                      &key &allow-other-keys)
  (if (stringp (first utterance))
    (de-render utterance :de-render-string-meets)
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(instantiate-form-constraints utterance))
                                      (syn-cat ())))
		   :right-pole '((root)))))

(defun instantiate-form-constraints (list-of-form-constraints)
  (loop for fc in list-of-form-constraints
        collect (loop for el in fc
                      if (variable-p el)
                      collect (intern (subseq (symbol-name el) 1))
                      else
                      collect el)))

(defun remove-quotes+full-stops (utterance)
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          unless (member word '("\"" ".") :test #'string=)
          collect (downcase word))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-ignore-quotes+full-stops))
                      &key &allow-other-keys)
  (de-render (remove-quotes+full-stops utterance) :de-render-string-meets-ignore-quotes+full-stops))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-ignore-quotes+full-stops))
                      &key &allow-other-keys)
  (de-render utterance :de-render-string-meets))

(in-package :grammar-learning)


(defun empty-cxn-set (experiment)
  (let* ((grammar-name (make-const "clevr-learning-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions
                     ,grammar-name
                   :cxn-inventory ,grammar-name
                   :hashed t
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:node-tests :restrict-nr-of-nodes :restrict-search-depth :check-duplicate)
                                        (:cxn-supplier-mode . ,(get-configuration experiment :learner-cxn-supplier))
                                        (:parse-goal-tests :no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure :non-gold-standard-meaning)
                                        (:de-render-mode . ,(get-configuration experiment :de-render-mode))
                                        (:parse-order routine)
                                        (:max-nr-of-nodes . ,(get-configuration experiment :max-nr-of-nodes))
                                        (:original-max-nr-of-nodes . ,(get-configuration experiment :max-nr-of-nodes))
                                        (:production-order routine)
                                        (:mark-holophrases . ,(get-configuration experiment :mark-holophrases))
                                        (:meaning-representation-formalism . ,(get-configuration experiment :meaning-representation))
                                        (:render-mode . :generate-and-test)
                                        (:category-linking-mode . ,(get-configuration experiment :category-linking-mode))
                                        (:update-categorial-links . t)
                                        (:consolidate-repairs . t)
                                        (:use-meta-layer . t)
                                        (:initial-cxn-score . ,(get-configuration experiment :initial-cxn-score))
                                        (:initial-categorial-link-weight . ,(get-configuration experiment :initial-categorial-link-weight))
                                        (:ignore-transitive-closure . t)
                                        (:hash-mode . :hash-string-meaning))
                   :diagnostics (gl::diagnose-non-gold-standard-meaning gl::diagnose-non-gold-standard-utterance)
                   :repairs ,(get-configuration experiment :repairs)
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-categorial-network . t))))))
    cxn-inventory))

(defun handle-potential-holistic-cxn (form meaning parent-meaning cxn-inventory)
  (cond ((when (member 'add-categorial-links (repairs cxn-inventory) :key #'type-of)
         (do-create-categorial-links form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        ((when (member 'item-based->item-based--substitution (repairs cxn-inventory) :key #'type-of)
         (do-create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        ((when (member 'item-based->holistic (repairs cxn-inventory) :key #'type-of)
         (do-create-holistic-cxn-from-partial-analysis form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        ((when (member 'holistic->item-based--substitution (repairs cxn-inventory) :key #'type-of)
             (do-repair-holophrase->item-based+holistic+holistic--substitution form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        
        ((when (member 'holistic->item-based--addition (repairs cxn-inventory) :key #'type-of)
         (do-repair-holophrase->item-based+holistic--addition form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        ((when (member 'holistic->item-based (repairs cxn-inventory) :key #'type-of)
         (do-create-item-based-cxn-from-partial-holistic-analysis form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil)))
        (t
         (do-create-holistic-cxn form meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil))))

(define-event lexicon-changed)

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "increase the score of the cxn"
  (incf (attr-val cxn :score) delta)
  ;(when (> (attr-val cxn :score) upper-bound)
  ;  (setf (attr-val cxn :score) upper-bound))
  cxn)

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0))
  "decrease the score of the cxn."
  (let* ((alter-ego-cxn (alter-ego-cxn cxn (grammar agent)))
         (current-score (attr-val cxn :score))
         (new-score (if (> current-score 1.0)
                      (- current-score (* current-score delta))
                      (- current-score delta))))
  (unless alter-ego-cxn
    (+ 1))
  (setf (attr-val cxn :score) new-score)
  (setf (attr-val alter-ego-cxn :score) new-score)
  (when (and (get-configuration (experiment agent) :remove-cxn-on-lower-bound)
             (<= (attr-val cxn :score) lower-bound))
    (delete-cxn (name cxn) (grammar agent) :key #'name)
    (delete-cxn (name alter-ego-cxn) (grammar agent)) :key #'name)))






  ;(when (<= (attr-val cxn :score) lower-bound)
  ;  (if (get-configuration (experiment agent) :remove-cxn-on-lower-bound) 
  ;    (progn (notify lexicon-changed)
  ;      (with-disabled-monitor-notifications
  ;        (delete-cxn-and-th-node cxn agent)))
  ;    (setf (attr-val cxn :score) lower-bound)))
  ;;(grammar agent))

(defun delete-cxn-and-th-node (cxn agent)
  (let ((lex-class
         (loop for unit in (contributing-part cxn)
               for lex-class = (gl::lex-class-item-based unit)
               when lex-class return lex-class))
        (cat-net (categorial-network (grammar agent))))
    (delete-cxn cxn (grammar agent))
    (notify lexicon-changed)
    (when lex-class
      (remove-category lex-class cat-net))))

;;;;  COMPETITORS
;;;; -------------


;; punish abstract cxns too!
;; e.g. what ?X is the ?Y ?Z was not used, but what size is the ?X ?Y was used instead,
;; 

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
         (item-based-competitors
          (loop for comp in (get-cxns-of-type agent 'gl::item-based) ;; also skips cxns with score 0
                for comp-name-with-placeholders =
                (gl::make-cxn-placeholder-name
                 (extract-form-predicates comp)
                 cxn-inventory)
                when (and (length= cxn-name-with-placeholders
                                   comp-name-with-placeholders)
                          (loop for cxn-elem in cxn-name-with-placeholders
                                for comp-elem in comp-name-with-placeholders
                                for i from 0
                                always (or (string= cxn-elem comp-elem) ; either the string is equal to the string of the comparing cxn
                                           (and (placeholderp cxn-elem) ; or both parts are variables ==> to do: do check in TH
                                                (placeholderp comp-elem))
                                           (and (placeholderp cxn-elem) ; or one part is a variable
                                                (string= comp-elem (nth i de-rendered-utterance))) ; the other part matches the nth word from the utterance
                                           (and (placeholderp comp-elem) ; competitor part is a variable (more abstract)
                                                (stringp cxn-elem))))) ; and the cxn part is a string
                collect comp))
         (holophrase-competitors
          (loop for other-cxn in (constructions-list cxn-inventory)
                when (and (eql (get-cxn-type other-cxn) 'gl::holistic)
                          (string= (extract-and-render other-cxn)
                                   (list-of-strings->string
                                    (fcg::tokenize utterance))))
                collect other-cxn)))
    (remove cxn (append holophrase-competitors item-based-competitors))))

(defun placeholderp (str)
  (eql (char str 0) #\?))


(defun get-meaning-competitors (agent applied-cxns utterance)
  "Get cxns with the same form as cxn"
  (loop for cxn in applied-cxns
        for cxn-type = (get-cxn-type cxn)
        for alter-ego = (alter-ego-cxn cxn (grammar agent))
        for competitors = (when (eql cxn-type 'gl::item-based)
                            (remove alter-ego (meaning-competitors-for-cxn-type
                                               cxn (grammar agent) cxn-type
                                               agent utterance)))
        append competitors))
