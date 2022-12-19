(in-package :naming-game)

(defun make-word ()
  "Creates a word of nr-of-syllables where a syllable is a consonant plus a vowel"
  (let ((vowels '("a" "e" "i" "o" "u"))
        (consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
        (word ""))
    (when (or (member word *all-words* :test #'string=) (string= word ""))
      (setq word "")
      (loop for i from 1 to 3
            do (setq word (concatenate 'string word (nth (random (length consonants)) consonants)))
               (setq word (concatenate 'string word (nth (random (length vowels)) vowels)))))
    word))

; 'voc-item' class for each lexical item in an agent's vocabulary:
(defclass voc-item ()
  ((form :documentation "the form that is produced to refer to the object" :type string :accessor form :initarg :form)
   (meaning :documentation "the object that is refered to by the form" :type symbol :accessor meaning :initarg :meaning)
   (score :documentation "the score that is associated to the word" :type float :accessor score :initarg :score)))


(defmethod is-equal ((item-1 voc-item)(item-2 voc-item))
  "Checks if item-1 is equal to item-2"
  (and (eql (form item-1) (form item-2))(eql (meaning item-1) (meaning item-2))))

(defun increase-score (applied-cxn delta upper-bound)
  "Increases score of voc-item with delta and cuts it off at upper-bound"
  (incf (cdr (second (attributes applied-cxn))) delta)
  (if (> (cdr (second (attributes applied-cxn))) upper-bound)
    (setf (cdr (second (attributes applied-cxn))) upper-bound)))

(defun decrease-score (applied-cxn delta lower-bound)
  "Decreases score of voc-item with delta and cuts it off at lower-bound"
  (decf (cdr (second (attributes applied-cxn))) delta)
  (if (<= (cdr (second (attributes applied-cxn))) lower-bound)
    (setf (cdr (second (attributes applied-cxn))) lower-bound)))

(defun get-form-competitors (agent)
  "pick different forms from of lexicon that have the same meaning as voc-item"
  (let* ((lexicon (lexicon agent))
        (topic (topic agent))
        (utterance (utterance agent))
        (form-competitors (loop for cxn in (constructions lexicon)
                                when (and (NOT (string= (cdr (assoc :form (attributes cxn))) utterance))
                                      (eql (first (cdr (assoc :meaning (attributes cxn)))) topic))
                                  collect cxn)))
    form-competitors))

