(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;
;; List Utilities ;;
;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converting between categories, strings, meanings and lexical-units ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Combining stuff
;;;;;;;;;;;;;;;;;;

(defun get-string-cat-meaning-tuples-for-utterance (utterance lexical-units)
  "Returns a list of (string category meaning) tuples, one for each word in utterance."
  (mapcar #'(lambda (word)
              (get-string-cat-meaning-tuple-for-word word lexical-units)) utterance))

(defun get-string-cat-meaning-tuple-for-word (string lexical-units)
  "Returns a (string category meaning) tuple for word given lexical-units."
  (list string (string->category string lexical-units) (string->meaning string lexical-units)))

(defun get-string-cat-meaning-tuple-for-word-and-marker (string lexical-units marker)
  "Returns a (string category meaning) tuple for word given lexical-units."
  (list string (string->category string lexical-units) (string->meaning string lexical-units) marker))

(defun markerp (word)
  (and (search "-" word)
       (= (search "-" word) 0)))

(defun get-string-cat-meaning-marker-tuples-for-utterance (utterance lexical-units)
  "Returns a list of (string category meaning) tuples, one for each word in utterance."
  (remove nil (loop for word in utterance
                    for i from 0
                    for next-word = (when (<= (+ i 1) (length utterance))
                                      (nth (+ i 1) utterance))
                    if (markerp next-word)
        
                    collect (get-string-cat-meaning-tuple-for-word-and-marker word lexical-units next-word)
                    else collect
                    (unless (markerp word)
                      (get-string-cat-meaning-tuple-for-word word lexical-units)))))
        

;; Removing non-lexical-constructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-lexical-constructions (list-of-constructions)
  "Removes lexical constructions from list-of-constructions"
  (remove-if #'(lambda (label)
                 (equalp "LEX" (symbol-name label)))
             list-of-constructions
             :key (lambda (cxn) (attr-val cxn :label))))

(defun lexical-cxn-p (cxn)
  (equalp "LEX" (symbol-name (attr-val cxn :label))))

;; Removing non-word-units
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-non-word-units (list-of-units)
  "Remove all units who have not a feature unit-type word"
  (remove-if-not #'(lambda (unit) (string= (unit-feature-value unit 'unit-type) 'word)) list-of-units))

(defun remove-grammatical-units (list-of-units)
  "Remove all units that have a unit-type that is not word"
  (remove-if-not #'(lambda (unit) (or (eq (unit-feature-value unit 'unit-type) nil)
                                      (string= (unit-feature-value unit 'unit-type) 'word))) list-of-units))

;; Starting from lexical unit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lexical-unit->category (lexical-unit)
  "Returns the category of a lexical-unit."
  (second (first (unit-feature-value lexical-unit 'syn-cat))))

(defun lexical-unit->meaning (lexical-unit)
  "Returns the meaning of a lexical-unit (as in yellow)."
  (first (first (unit-feature-value lexical-unit 'meaning))))

(defun lexical-unit->string (lexical-unit)
  "Returns the string in a lexical unit."
  (third (car (unit-feature-value lexical-unit 'form))))

(defun lexical-unit->object-id (lexical-unit)
  (second (first (unit-feature-value lexical-unit 'meaning))))

(defun cxn-unit->category (cxn-unit)
  (second (second (unit-feature-value cxn-unit 'syn-cat))))

;; Starting from category
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun category->lexical-unit (category lexical-units)
  "Returns the lexical unit that contains the category."
  (loop for unit in lexical-units
        when (equalp category (lexical-unit->category unit))
        return unit))

(defun category->string (category lexical-units)
  "Returns the string of the word of the category."
  (lexical-unit->string (category->lexical-unit category lexical-units)))

(defun category->meaning (category lexical-units)
  "Returns the meaning of the word of the category."
  (lexical-unit->meaning (category->lexical-unit category lexical-units)))

;; Starting from meaning
;;;;;;;;;;;;;;;;;;;;;;;;

(defun meaning->lexical-unit (meaning lexical-units)
  "Returns the lexical unit that contains the meaning."
  (loop for unit in lexical-units
        when (equalp meaning (lexical-unit->meaning unit))
        return unit))

(defun meaning->string (meaning lexical-units)
  "Returns the string of the word with the meaning."
  (lexical-unit->string (meaning->lexical-unit meaning lexical-units)))

(defun meaning->category (meaning lexical-units)
  "Returns the category of the word with the meaning."
  (lexical-unit->category (meaning->lexical-unit meaning lexical-units)))

;; Starting from string
;;;;;;;;;;;;;;;;;;;;;;;

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

;; Testing everything
;;;;;;;;;;;;;;;;;;;;;

(deftest lexical-unit-conversion ()
    (let ((lexical-units '((GEEL-4 (MEANING ((YELLOW ?X-236)))
                                   (FORM ((STRING GEEL-4 "geel")))
                                   (UNIT-TYPE WORD)
                                   (ARGS (?X-236))
                                   (SYN-CAT ((LEX-CLASS CAT-13))))
                           (CIRKEL-4 (MEANING ((CIRCLE ?X-233)))
                                     (FORM ((STRING CIRKEL-4 "cirkel")))
                                     (UNIT-TYPE WORD)
                                     (ARGS (?X-233))
                                     (SYN-CAT ((LEX-CLASS CAT-10))))
                           (VIERKANT-4 (MEANING ((SQUARE ?X-235)))
                                       (FORM ((STRING VIERKANT-4 "vierkant")))
                                       (UNIT-TYPE WORD)
                                       (ARGS (?X-235))
                                       (SYN-CAT ((LEX-CLASS CAT-9))))
                           (REUSACHTIG-4 (MEANING ((HUGE ?X-237)))
                                         (FORM ((STRING REUSACHTIG-4 "reusachtig")))
                                         (UNIT-TYPE WORD)
                                         (ARGS (?X-237))
                                         (SYN-CAT ((LEX-CLASS CAT-16)))))))
      ;; Starting from lexical-unit
      (test-equalp "cirkel" (lexical-unit->string (second lexical-units)))
      (test-equalp 'square (lexical-unit->meaning (third lexical-units)))
      (test-equalp ' cat-16 (lexical-unit->category (fourth lexical-units)))
      ;; Starting from string
      (test-equal '(GEEL-4 (MEANING ((YELLOW ?X-236)))
                                   (FORM ((STRING GEEL-4 "geel")))
                                   (UNIT-TYPE WORD)
                                   (ARGS (?X-236))
                                   (SYN-CAT ((LEX-CLASS CAT-13))))
                (string->lexical-unit "geel" lexical-units))
      (test-equalp 'square (string->meaning "vierkant" lexical-units))
      (test-equalp 'cat-16 (string->category "reusachtig" lexical-units))
      ;; Starting from meaning
       (test-equal '(GEEL-4 (MEANING ((YELLOW ?X-236)))
                                   (FORM ((STRING GEEL-4 "geel")))
                                   (UNIT-TYPE WORD)
                                   (ARGS (?X-236))
                                   (SYN-CAT ((LEX-CLASS CAT-13))))
                (meaning->lexical-unit 'yellow lexical-units))
      (test-equalp "vierkant" (meaning->string 'square lexical-units))
      (test-equalp 'cat-16 (meaning->category 'huge lexical-units))
      ;; Starting from category
       (test-equal '(GEEL-4 (MEANING ((YELLOW ?X-236)))
                                   (FORM ((STRING GEEL-4 "geel")))
                                   (UNIT-TYPE WORD)
                                   (ARGS (?X-236))
                                   (SYN-CAT ((LEX-CLASS CAT-13))))
                (category->lexical-unit 'cat-13 lexical-units))
      (test-equalp 'square (category->meaning 'cat-9 lexical-units))
      (test-equalp "cirkel" (category->string 'cat-10 lexical-units))))

;; (lexical-unit-conversion)
