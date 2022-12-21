(in-package :naming-game)

;------------------------;
;manages words and scores;
;------------------------;

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

