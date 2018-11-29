
(in-package :nao-interface)

(export '(nao-detect-touch nao-head-yes-no))

#|
(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :front)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Front"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :middle)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Middle"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :rear)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Rear"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

(defmethod nao-head-yes-no ((nao nao))
  (let* ((json (make-json 'head-touch :data '((action . "front-back"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :detected :value 1 :test #'=)))
|#

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