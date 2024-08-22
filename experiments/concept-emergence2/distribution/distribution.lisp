(in-package :cle)

;; ------------------
;; + Abstract class +
;; ------------------

(defclass distribution ()
  ()
  (:documentation "Abstract class for distributions"))


(defmethod make-new-distribution (agent channel observation)
  (let* ((world (world (experiment agent))))
    (if (channel-continuous-p world channel)
      (make-distribution agent observation (get-configuration (experiment agent) :distribution))
      (make-distribution agent observation :categorical))))
