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
      (make-distribution agent observation (get-configuration (experiment agent) :distribution))
      (make-distribution agent observation :categorical))))
