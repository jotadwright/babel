(in-package :pattern-finding-old)

;;;; Success Buffer
(defun get-windowed-success (experiment)
  (let* ((buffer-size (get-configuration experiment :buffer-size))
         (interactions (first-n buffer-size (interactions experiment)))
         (successes (mapcar #'communicated-successfully interactions)))
    (/ (count t successes) (length interactions))))

(defun get-overall-success (experiment)
  (let ((successes (mapcar #'communicated-successfully (interactions experiment))))
    (/ (count t successes) (length (interactions experiment)))))

;;;; Repair buffer
;; access :applied-repair label in interactions

;;;; Failed corpus buffer
;; access :utterance and :gold-standard-meaning label in interactions
;; together with communicated-successfully slot

(defun add-cxns-to-wi (constructions)
  (dolist (cxn constructions)
    (add-element (make-html cxn))))