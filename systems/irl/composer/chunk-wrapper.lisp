(in-package :irl)

;; #########################################
;; run chunk wrapper fn
;; -----------------------------------------

(defun run-chunk-wrapper (chunk composer)
  (let ((mode (get-configuration composer :chunk-wrapper)))
    (wrap-chunk chunk composer mode)))

(defgeneric wrap-chunk (chunk composer mode)
  (:documentation "A function that is called before chunk evaluation
    to add things to the irl program. Gets the chunk
    and returns a new chunk or nil when the wrapping
    failed. When the wrapping fails, add 'chunk-wrapper-failed
    to the statuses of the node."))

(defmethod wrap-chunk ((chunk chunk) (composer chunk-composer)
                       (mode (eql :identity)))
  (declare (ignorable composer))
  chunk)
  