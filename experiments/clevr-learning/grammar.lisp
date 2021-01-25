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

(defun empty-cxn-set (hide-type-hierarchy)
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
                   :fcg-configurations ((:cxn-supplier-mode . :ordered-by-label-and-score)
                                        (:parse-order lexical item-based holophrase)
                                        (:parse-goal-tests :no-applicable-cxns
                                                           :connected-semantic-network
                                                           :no-strings-in-root)
                                        (:de-render-mode . :de-render-string-meets-no-punct)
                                        (:th-connected-mode . :path-exists) ;; this skips the add-th-links repair
                                        (:update-th-links . t))
                   :visualization-configurations ((:show-constructional-dependencies . nil)
                                                  (:show-type-hierarchy . ,(not hide-type-hierarchy)))))))
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
          (delete-cxn-and-th-node cxn (grammar agent))))
      (setf (attr-val cxn :score) lower-bound)))
  (grammar agent))

(defun delete-cxn-and-th-node (cxn cxn-inventory)
  (let ((lex-class (gl::lex-class-cxn cxn))
        (type-hierarchy (get-type-hierarchy cxn-inventory)))
    (delete-cxn cxn cxn-inventory)
    (notify lexicon-changed)
    (when lex-class
      (graph-utils::delete-node
       (type-hierarchies::graph type-hierarchy)
       lex-class))))

(defun get-form-competitors (agent applied-cxns)
  "Get cxns with the same meaning as cxn"
  (let ((all-cxns-with-meaning
         (remove-duplicates
          (loop for cxn in applied-cxns
                for cxn-meaning = (extract-meaning-predicates cxn)
                append (remove cxn
                               (find-all cxn-meaning
                                         (constructions-list (grammar agent))
                                         :key #'extract-meaning-predicates
                                         :test #'unify-irl-programs))))))
    (loop for cxn in applied-cxns
          do (setf all-cxns-with-meaning
                   (remove cxn all-cxns-with-meaning)))
    all-cxns-with-meaning))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'holophrase)))
  ;; holophrase competitors have exactly the same form
  (flet ((extract-and-render (cxn)
           (list-of-strings->string
            (render (extract-form-predicates cxn)
                    (get-configuration cxn-inventory :render-mode)))))
    (let* ((all-cxns-of-type
            (remove cxn
                    (find-all cxn-type (constructions-list cxn-inventory)
                              :key #'get-cxn-type)))
           (cxn-form (extract-and-render cxn))
           (competitors
            (find-all cxn-form all-cxns-of-type
                      :key #'extract-and-render
                      :test #'string=)))
      competitors)))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'lexical)))
  ;; lexical competitors have exactly the same form
  (flet ((extract-and-render (cxn)
           (list-of-strings->string
            (render (extract-form-predicates cxn)
                    (get-configuration cxn-inventory :render-mode)))))
    (let* ((all-cxns-of-type
            (remove cxn
                    (find-all cxn-type (constructions-list cxn-inventory)
                              :key #'get-cxn-type)))
           (cxn-form (extract-and-render cxn))
           (competitors
            (find-all cxn-form all-cxns-of-type
                      :key #'extract-and-render
                      :test #'string=)))
      competitors)))

(defmethod meaning-competitors-for-cxn-type ((cxn construction)
                                             (cxn-inventory construction-inventory)
                                             (cxn-type (eql 'item-based)))
  ;; item-based competitors have unifiable form constraints
  (let* ((all-cxns-of-type
          (remove cxn
                  (find-all cxn-type (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (cxn-form (extract-form-predicates cxn))
         (competitors
          (find-all cxn-form all-cxns-of-type
                    :key #'extract-form-predicates
                    :test #'unify)))
      competitors))


(defun combined-meaning-competitors (agent cxn-inventory)
  ;; the holophrase from which the current sequence
  ;; of applied cxns originated can still exist.
  ;; When succesful, decrease its score
  (flet ((extract-and-render (cxn)
           (list-of-strings->string
            (render (extract-form-predicates cxn)
                    (get-configuration cxn-inventory :render-mode)))))
  (let ((holophrase-cxns
         (find-all 'holophrase (constructions-list cxn-inventory)
                   :key #'get-cxn-type))
        (processed-utterance
         (list-of-strings->string
          (fcg::tokenize (utterance agent)))))
    (loop for cxn in holophrase-cxns
          when (string= processed-utterance
                        (extract-and-render cxn))
          collect cxn))))
    
          

(defun get-meaning-competitors (agent applied-cxns)
  "Get cxns with the same form as cxn"
  (append
   ;; get competitors for each construction separately
   (loop for cxn in applied-cxns
         for cxn-type = (get-cxn-type cxn)
         for competitors = (meaning-competitors-for-cxn-type
                            cxn (grammar agent) cxn-type)
         append competitors)
   ;; get competitors for the combined applied cxns
   ;; (item-based + lexical might have a holophrase competitor)
   (when (length> applied-cxns 1)
     (combined-meaning-competitors agent (grammar agent)))))