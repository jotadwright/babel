(in-package :cle)


;; -----------------------
;; + Update distribution +
;; -----------------------

(defmethod update-prototype ((interaction-number number)
                             (prototype prototype)
                             (object cle-object)
                             &key &allow-other-keys)
  (let ((new-observation (get-channel-val object (channel prototype)))
        (distribution (distribution prototype)))
    (update-distribution new-observation distribution)
    (update-prototype-history interaction-number new-observation distribution)))

;; ------------------
;; + Update weights +
;; ------------------

(defgeneric update-weight (concept channel delta mode &key &allow-other-keys)
  (:documentation "Method by which to adjust the weight of a feature channel."))

(defmethod update-weight (concept channel delta (mode (eql :standard))
                                     &key
                                     (lower-bound 0.0)
                                     (upper-bound 1.0))
  "Standard update with a delta value."
  (let ((prototype (find channel (prototypes concept) :key #'channel)))
    ;; update the weight
    (setf (weight-val prototype) (+ (weight-val prototype) delta))
    ;; check the boundaries
    (when (> (weight-val prototype) upper-bound)
      (setf (weight-val prototype) upper-bound))
    (when (<= (weight prototype) lower-bound)
      (setf (weight-val prototype) lower-bound))))

(defmethod update-weight (concept channel reward (mode (eql :j-interpolation)) &key)
  "Apply the j-interpolation update rule to the weight of a specified channel of a given concept."
  (let* ((prototype (find channel (prototypes concept) :key #'channel))
         (old-value (weight-val prototype)))
    (setf (weight-val prototype) (+ old-value reward))))
