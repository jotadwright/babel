(in-package :irl-2)

;; #########################################
;; run check chunk evaluation result fn
;; -----------------------------------------

(defun run-check-chunk-evaluation-result-fns (result composer)
  (loop for mode in (get-configuration composer :check-chunk-evaluation-result-modes)
        always (check-chunk-evaluation-result result composer mode)))

(defgeneric check-chunk-evaluation-result (result composer mode)
  (:documentation "A function that is called after chunk
   evaluation. Returns t when the result is a good result"))

(defmethod check-chunk-evaluation-result ((result chunk-evaluation-result)
                                          (composer chunk-composer)
                                          (mode (eql :identity)))
  (declare (ignorable composer))
  result)

(defmethod check-chunk-evaluation-result ((result chunk-evaluation-result)
                                          (composer chunk-composer)
                                          (mode (eql :no-open-vars)))
  (declare (ignorable composer))
  (not (open-vars (chunk result))))