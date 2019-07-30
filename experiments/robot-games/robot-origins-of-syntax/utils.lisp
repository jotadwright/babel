;;;; /utils.lisp

(in-package :roos)

(defun generate-scene (agent)
  "Generate a random scene"
  (let ((context-size (get-configuration agent :context-size))
        (features (get-configuration agent :features))
        (feature-bounds (get-configuration agent :feature-bounds)))
    (loop repeat context-size
          for obj-feats = (loop for feature in features
                                for min-bound = (rest (assoc 'min (rest (assoc feature feature-bounds))))
                                for max-bound = (rest (assoc 'max (rest (assoc feature feature-bounds))))
                                for feature-val = (if (listp min-bound)
                                                    (loop repeat (length min-bound)
                                                          collect (random-from-range (first min-bound) (first max-bound))) 
                                                    (random-from-range min-bound max-bound))
                                collect (cons feature feature-val))
          for id = (make-id 'obj)
          collect (list (cons id obj-feats)))))

(defun get-channel (category)
  (case (type-of category)
    (xpos-category :xpos)
    (ypos-category :ypos)
    (area-category :area)
    (color-category :color)))

(defun remove-non-word-units (list-of-units)
  "Remove all units who have not a feature unit-type word"
  (remove-if-not #'(lambda (unit)
                     (string= (unit-feature-value unit 'unit-type) 'word))
                 list-of-units))

(defun remove-grammatical-units (list-of-units)
  "Remove all units that have a unit-type that is not word"
  (remove-if-not #'(lambda (unit)
                     (or (eq (unit-feature-value unit 'unit-type) nil)
                         (string= (unit-feature-value unit 'unit-type) 'word)))
                 list-of-units))

(defun remove-lexical-constructions (list-of-constructions)
  "Removes lexical constructions from list-of-constructions"
  (remove-if #'(lambda (label)
                 (equalp "LEX" (symbol-name label)))
             list-of-constructions
             :key (lambda (cxn) (attr-val cxn :label))))

(defun all-lexical-constructions (list-of-constructions)
  "Returns all lexical constructions from list-of-constructions"
  (remove-if-not #'(lambda (label)
                     (equalp "LEX" (symbol-name label)))
                 list-of-constructions
                 :key #'(lambda (cxn) (attr-val cxn :label))))

(defun lexical-cxn-p (cxn)
  (equalp "LEX" (symbol-name (attr-val cxn :label))))

(defun lexical-unit->category (lexical-unit)
  "Returns the category of a lexical-unit."
  (second (first (unit-feature-value lexical-unit 'syn-cat))))

(defun lexical-unit->object-id (lexical-unit)
  (last-elt (first (unit-feature-value lexical-unit 'meaning))))

(defun lexical-unit->meaning (lexical-unit)
  "Returns the meaning of a lexical-unit (as in yellow)."
  (second (first (unit-feature-value lexical-unit 'meaning))))

(defun lexical-unit->string (lexical-unit)
  "Returns the string in a lexical unit."
  (third (car (unit-feature-value lexical-unit 'form))))

(defun cxn-unit->category (cxn-unit)
  (second (second (unit-feature-value cxn-unit 'syn-cat))))

(defun string->lexical-unit (string lexical-units)
  "Returns the lexical unit that contains the form string."
  (loop for unit in lexical-units
        when (equalp string (lexical-unit->string unit))
        return unit))

(defun string->category (string lexical-units)
  "Returns the category of the word for containg string in lexical-units."
  (lexical-unit->category (string->lexical-unit string lexical-units)))

(defun string->meaning (string lexical-units)
  "Returns the meaning of the word for containg string in lexical-units."
  (lexical-unit->meaning (string->lexical-unit string lexical-units)))

(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set))
  "Two cxns are the same if they have the same name."
  (find-cxn cxn cxn-inventory :key #'name :test #'string=))

(defun get-string-cat-meaning-tuples-for-utterance (utterance lexical-units)
  "Returns a list of (string category meaning) tuples, one for each word in utterance."
  (mapcar #'(lambda (word)
              (get-string-cat-meaning-tuple-for-word word lexical-units)) utterance))

(defun get-string-cat-meaning-tuple-for-word (string lexical-units)
  "Returns a (string category meaning) tuple for word given lexical-units."
  (list string (string->category string lexical-units) (string->meaning string lexical-units)))

(defun all-subsequences (list)
  "Returns all continuous subsequences of a list."
  (labels ((subsequences (tail &optional (acc '()) (result '()))
             (if (endp tail)
               (list* (reverse acc) result)
               (subsequences (rest tail) (list* tail acc)
                             (append (subsequences (rest tail) acc) result))))
           (continuous-p (subsequence-d)
             "True if the designated subsequence is continuous."
             (loop for i in subsequence-d
                   for j on (first subsequence-d)
                   always (eq i j)))
           (designated-sequence (subsequence-d)
             "Destructively transforms a subsequence designator into
              the designated subsequence."
             (map-into subsequence-d 'first subsequence-d)))
    (let ((nc-subsequences (delete-if-not #'continuous-p (subsequences list))))
      (map-into nc-subsequences #'designated-sequence nc-subsequences))))