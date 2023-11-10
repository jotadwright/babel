(in-package :pf-for-sql)

;----------;
; Problems ;
;----------;

(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when the utterance is completely unknown
                      OR when no solution is found using partial utterances."))

(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when part of the utterance is known."))

(defclass failed-interpretation-problem (problem)
  () (:documentation "Problem created when interpretation has failed."))

(defclass partial-meaning-problem (problem)
  () (:documentation "Problem created when part of the meaning is known."))