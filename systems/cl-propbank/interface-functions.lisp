(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; Interface functions ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun find-pb-predicate (lemma)
  (cond ((symbolp lemma)
        (find lemma *pb-data* :key #'lemma :test #'equal))
        ((stringp lemma)
         (find lemma *pb-data* :key #'(lambda (o) (symbol-name (lemma o))) :test #'equalp))
        (t (error "Lemma should be of type string or symbol."))))

; (find-pb-predicate 'believe)
; (setf *a* (find-pb-predicate "feel"))


(defun find-roleset (roleset-id)
  (loop for predicate in *pb-data*
        for role-set = (find roleset-id (rolesets predicate) :key #'id)
        if role-set
        return it))

; (find-roleset 'feel.01)
; (setf *a* (find-roleset 'feel.01))