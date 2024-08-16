(in-package :cle)

;; ----------------------------------------
;; + Strategy to find meaning competitors +
;; ----------------------------------------

(defmethod decide-competitors-hearer (agent applied-cxn &key &allow-other-keys)
  "Determines the meaning competitors of a hearer agent: all similar concepts that are discriminative."
  (let* ((candidates (mapcar (lambda (x) (assqv :cxn x)) (search-discriminative-concepts agent)))
         (competitors (remove applied-cxn candidates :test #'(lambda (x y) (equal x y)))))
    (set-data agent 'meaning-competitors competitors)))
