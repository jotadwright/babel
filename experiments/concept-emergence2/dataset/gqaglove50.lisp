(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

;; GQA - GLOVE 50

(defun gqaglove50-channels ()
  "GQA-GLOVE50 dataset."
  (loop for i below 50 collect (intern (format nil "DIM-~a" i))))

(defmethod get-all-channels ((mode (eql :gqaglove50)))
  (gqaglove50-channels))

(defmethod is-channel-available ((mode (eql :gqaglove50)) symbolic-attribute raw-attributes)
  t)

;; GQA (3-10 objects) - GLOVE 50

(defmethod get-all-channels ((mode (eql :gqaglove50-small)))
  (gqaglove50-channels))

(defmethod is-channel-available ((mode (eql :gqaglove50-small)) symbolic-attribute raw-attributes)
  t)

;; MSCOCO IMAGES (3-10 objects) - DINO V2

(defun dinov2-channels ()
  "GQA-GLOVE50 dataset."
  (loop for i below 384 collect (intern (format nil "DIM~a" i))))

(defmethod get-all-channels ((mode (eql :dinov2)))
  (dinov2-channels))

(defmethod is-channel-available ((mode (eql :dinov2)) symbolic-attribute raw-attributes)
  t)

(defmethod get-all-channels ((mode (eql :dinov2-50)))
  (dinov2-channels))

(defmethod is-channel-available ((mode (eql :dinov2-50)) symbolic-attribute raw-attributes)
  t)
