
(in-package :nao-interface)

(export '(nao-detect-touch nao-head-yes-no))

;; + nao detect touch +

(defgeneric nao-detect-touch (nao region sensor)
  (:documentation "Detect touch on one of the sensors of the Nao.
                   Region must be from '(:head :chest :hand :feet)
                   Sensor depends on Region"))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :front)))
  (let* ((data '((region . "Front")))
         (response (nao-send-http nao "/headtouch/detect" :data data)))
    (rest (assoc :success response))))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :middle)))
  (let* ((data '((region . "Middle")))
         (response (nao-send-http nao "/headtouch/detect" :data data)))
    (rest (assoc :success response))))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :rear)))
  (let* ((data '((region . "Rear")))
         (response (nao-send-http nao "/headtouch/detect" :data data)))
    (rest (assoc :success response))))

;; + head yes or no +

(defgeneric nao-head-yes-no (nao)
  (:documentation "Detect if the Nao's head is touched in the front or in the back"))

(defmethod nao-head-yes-no ((nao nao))
  (let ((response (nao-send-http nao "/headtouch/yes_no")))
    (string= (rest (assoc :result response)) "front")))