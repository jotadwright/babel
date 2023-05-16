(in-package :ecl)

;; -----------------
;; + Update weights +
;; -----------------

(defgeneric update-weight (concept attribute delta mode &key &allow-other-keys)
  (:documentation "Method by which to adjust the weight of a feature channel."))

(defmethod update-weight (concept attribute delta (mode (eql :standard))
                                     &key
                                     (lower-bound 0.0)
                                     (upper-bound 1.0))
  "Standard update with a delta value."
  (let ((prototype (find attribute (meaning concept) :key #'attribute)))
    ;; update the weight
    (setf (weight-value prototype) (+ (weight-value prototype) delta))
    ;; check the boundaries
    (when (> (weight-value prototype) upper-bound)
      (setf (weight-value prototype) upper-bound))
    (when (<= (weight-value prototype) lower-bound)
      (setf (weight-value prototype) lower-bound))))

(defmethod update-weight (concept attribute reward (mode (eql :j-interpolation)) &key)
  "Apply the j-interpolation update rule to the weight of a specified attribute of a given concept."
  (let* ((prototype (find attribute (meaning concept) :key #'attribute))
         (old-value (weight-value prototype)))
    (setf (weight-value prototype) (+ old-value reward))))
