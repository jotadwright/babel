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
  (when (attr-val construction :lemma)
    (mapcar #'symbol-name (remove nil (list (attr-val construction :lemma))))))


(defmethod hash ((node cip-node)
                 (mode (eql :hash-lemma)) 
                 &key &allow-other-keys)
  "Checks all units for a lemma feature."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lemma = (if (equalp (unit-feature-value unit 'node-type) 'leaf)
                      (unit-feature-value unit 'lemma)
                      (or (unit-feature-value unit 'lemma) ;;for phrasals
                          (unit-feature-value unit 'string)))
        when (and lemma (symbolp lemma))
          collect (symbol-name lemma)
        when (and lemma (stringp lemma))
          collect lemma))