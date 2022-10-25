(in-package :irl)

;; #########################################
;; run-chunk-evaluation-goal-tests
;; -----------------------------------------

(defun run-chunk-evaluation-goal-tests (evaluation-result composer)
  (loop for mode in (get-configuration composer :chunk-evaluation-goal-tests)
        always (chunk-evaluation-goal-test evaluation-result composer mode)))

(defgeneric chunk-evaluation-goal-test (evaluation-result composer mode)
  (:documentation "A function that is called after chunk
   evaluation. Returns t when the result is a good result"))

(defmethod chunk-evaluation-goal-test ((result chunk-evaluation-result)
                                       (composer chunk-composer)
                                       (mode (eql :identity)))
  (declare (ignorable composer))
  result)

(defmethod chunk-evaluation-goal-test ((result chunk-evaluation-result)
                                       (composer chunk-composer)
                                       (mode (eql :no-open-vars)))
  (declare (ignorable composer))
  (not (open-vars (chunk result))))

