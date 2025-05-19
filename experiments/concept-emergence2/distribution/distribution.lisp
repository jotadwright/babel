(in-package :cle)

;; ------------------
;; + Abstract class +
;; ------------------

(defclass distribution ()
  ()
  (:documentation "Abstract class for distributions"))


(defmethod make-new-distribution (agent channel observation)
  (let* ((world (world (experiment agent)))
         (view-name (current-view agent)))
    (if (channel-continuous-p world view-name channel)
      (make-distribution agent observation :gaussian-welford)
      (make-distribution agent observation :categorical))))
