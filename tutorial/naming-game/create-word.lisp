(in-package :naming-game)

(defun make-word (experiment)
  "Creates a word of nr-of-syllables where a syllable is a consonant plus a vowel"
  (let ((vowels '("a" "e" "i" "o" "u"))
        (consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))
        (word ""))
    (loop for i from 1 to 3
          do (setq word (concatenate 'string word (nth (random (length consonants)) consonants)))
          do (setq word (concatenate 'string word (nth (random (length vowels)) vowels))))
    word))

; 'voc-item' class for each lexical item in an agent's vocabulary:
(defclass voc-item ()
  ((form :documentation "the form that is produced to refer to the object" :type string :accessor form :initarg :form)
   (meaning :documentation "the object that is refered to by the form" :type symbol :accessor meaning :initarg :meaning)
   (score :documentation "the score that is associated to the word" :type float :accessor score :initarg :score)))


(defmethod is-equal ((item-1 voc-item)(item-2 voc-item))
  "Checks if item-1 is equal to item-2"
  (and (eql (form item-1) (form item-2))(eql (meaning item-1) (meaning item-2))))

(defmethod increase-score ((voc-item voc-item) (delta float) (upper-bound float))
  "Increases score of voc-item with delta and cuts it off at upper-bound"
  (incf (score voc-item) delta)
  (if (> (score voc-item) upper-bound)
      (setf (score voc-item) upper-bound)))

(defmethod decrease-score ((voc-item voc-item) (delta float) (lower-bound float))
  "Decreases score of voc-item with delta and cuts it off at lower-bound"
  (decf (score voc-item) delta)
  (if (<= (score voc-item) lower-bound)
      (setf (score voc-item) lower-bound)))

(defmethod get-form-competitors ((voc-item voc-item) (lexicon list))
  "pick different forms from of lexicon that have the same meaning as voc-item"
  (let ((form-competitors '()))
    (loop for item in lexicon
          do (if (and (eql (meaning item) (meaning voc-item)) (NOT (eql item voc-item)))
                 (push item form-competitors)))
    form-competitors))
