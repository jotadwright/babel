(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Hash Mode             ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod hash ((construction construction)
                 (mode (eql :hash-lemma))
                 &key &allow-other-keys)
  "Returns the lemma from the attributes of the construction"
  (cond ((attr-val construction :lemma)
         (remove nil (list (attr-val construction :lemma))))
        ((attr-val construction :gram-category)
         (remove nil (list (attr-val construction :gram-category))))
        (t
         nil)))


(defmethod hash ((node cip-node)
                 (mode (eql :hash-lemma)) 
                 &key &allow-other-keys)
  "Checks all units for a lemma feature."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lemma = (if (equalp (unit-feature-value unit 'node-type) 'leaf)
                      (unit-feature-value unit 'lemma)
                      (or (unit-feature-value unit 'lemma) ;;for phrasals
                          (intern (upcase (unit-feature-value unit 'string)))))
        when lemma
        collect it))