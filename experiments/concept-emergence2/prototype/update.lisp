(in-package :cle)

;; -----------------------
;; + Update distribution +
;; -----------------------

(defmethod update-prototype ((new-observation number)
                             (interaction-number number)
                             (prototype prototype)
                             &key (save-distribution-history t)
                             &allow-other-keys)
  "Update the distribution of a prototype."
  ;; update distribution
  (update-distribution new-observation (distribution prototype))
  ;; update history (for visualisation purposed or debugging)
  (when save-distribution-history
    (update-distribution-history interaction-number
                                 new-observation
                                 (distribution prototype))))

;; ------------------
;; + Update weights +
;; ------------------
(defgeneric update-weight (prototype delta mode &key &allow-other-keys)
  (:documentation "Method by which to adjust the weight of a feature channel."))

(defmethod update-weight (prototype delta (mode (eql :standard))
                                    &key
                                    (lower-bound 0.0)
                                    (upper-bound 1.0))
  "Standard update with a delta value."
  ;; update the weight
  (setf (weight-val prototype) (+ (weight-val prototype) delta))
  ;; check the boundaries
  (when (> (weight-val prototype) upper-bound)
    (setf (weight-val prototype) upper-bound))
  (when (<= (weight prototype) lower-bound)
    (setf (weight-val prototype) lower-bound)))

(defmethod update-weight (prototype reward (mode (eql :j-interpolation)) &key)
  "Apply the j-interpolation update rule to the weight of a specified channel of a given concept."
  (let* ((old-value (weight-val prototype)))
    (setf (weight-val prototype) (+ old-value reward))))

(defmethod update-history-weight ((agent cle-agent) (prototype prototype) (delta number))
  "Keeps track how many times and when the cxn is used."
  (let ((scene-idx (index (current-scene (world (experiment agent)))))
        (interaction-number (interaction-number (current-interaction (experiment agent)))))
    (setf (history prototype) (cons (list interaction-number scene-idx delta) (history prototype)))))
