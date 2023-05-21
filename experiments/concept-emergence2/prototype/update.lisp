(in-package :cle)


;; -----------------------
;; + Update distribution +
;; -----------------------

(defmethod update-prototype ((interaction-number number)
                             (prototype prototype)
                             (object cle-object)
                             (mode (eql :gaussian-welford))
                             &key &allow-other-keys)

  (let ((channel (channel prototype))
        (new-observation (get-channel-val object))
        (distribution (distribution prototype)))
    (welford-update new-observation distribution)))

;; ------------------
;; + Update weights +
;; ------------------

(defgeneric update-weight (concept attribute delta mode &key &allow-other-keys)
  (:documentation "Method by which to adjust the weight of a feature channel."))

(defmethod update-weight (concept attribute delta (mode (eql :standard))
                                     &key
                                     (lower-bound 0.0)
                                     (upper-bound 1.0))
  "Standard update with a delta value."
  (let ((prototype (find attribute (meaning concept) :key #'attribute)))
    ;; update the weight
    (setf (weight prototype) (+ (weight prototype) delta))
    ;; check the boundaries
    (when (> (weight prototype) upper-bound)
      (setf (weight prototype) upper-bound))
    (when (<= (weight prototype) lower-bound)
      (setf (weight prototype) lower-bound))))

(defmethod update-weight (concept attribute reward (mode (eql :j-interpolation)) &key)
  "Apply the j-interpolation update rule to the weight of a specified attribute of a given concept."
  (let* ((prototype (find attribute (meaning concept) :key #'attribute))
         (old-value (weight prototype)))
    (setf (weight prototype) (+ old-value reward))))
