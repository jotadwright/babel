(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; Searching in PropBank annotations.                               ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-corpus (corpus-name)
  (or (gethash corpus-name *ccxg-explorer-annotations*)
      (error "Corpus not found.")))

(defun find-by-schema (corpus-name schema &key (collect-fn #'identity) (max-n 10))
  (loop with n = 0
        while (< n max-n)
        for annotation in (get-corpus corpus-name)
        if (unify schema (second (assoc :roles annotation)))
        do (incf n)
        and
        collect (funcall collect-fn annotation)))