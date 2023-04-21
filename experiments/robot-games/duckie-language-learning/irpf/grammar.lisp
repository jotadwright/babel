;;;; grammar.lisp

(in-package :fcg)
(export '(make-duckie-grammar-cxns))

(def-fcg-constructions duckie-grammar
  :hashed t
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:parse-goal-tests :no-strings-in-root :connected-semantic-network :correct-interpretation)
                       (:de-render-mode . :de-render-string-meets-no-punct)
                       (:consolidate-repairs . t))
  :diagnostics (dll::diagnose-failed-interpretation
                dll::diagnose-unknown-utterance
                dll::diagnose-partial-utterance)
  :repairs (dll::holophrase->item-based--substitution
            dll::holophrase->item-based--addition
            dll::holophrase->item-based--deletion
            dll::add-holophrase
            dll::item-based->lexical
            dll::lexical->item-based
            dll::add-categorial-links)
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))

(defun detach-punctuation (word)
  "This function will check if the input string (word)
   has a punctuation at the end of it 
   and return the word without the punctuation in a list
   'bolima?' -> '(bolima)'"
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