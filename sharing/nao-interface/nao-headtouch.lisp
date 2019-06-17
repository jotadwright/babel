(in-package :nao-interface)

(export '(detect-head-touch yes-no-feedback))

;;;; Detect head touch
(defmethod detect-head-touch ((nao nao) (sensor (eql :front)))
  (rest (assoc :success
               (nao-send-http nao
                              :endpoint "/headtouch/detect"
                              :data '((region . "Front"))))))

(defmethod detect-head-touch ((nao nao) (sensor (eql :middle)))
  (rest (assoc :success
               (nao-send-http nao
                              :endpoint "/headtouch/detect"
                              :data '((region . "Middle"))))))

(defmethod detect-head-touch ((nao nao) (sensor (eql :rear)))
  (rest (assoc :success
               (nao-send-http nao
                              :endpoint "/headtouch/detect"
                              :data '((region . "Rear"))))))

(defmethod detect-head-touch ((nao nao) sensor)
  (error "The sensor ~a is not a valid head sensor for Nao" sensor))

;;;; yes/no feedback
(defmethod yes-no-feedback ((nao nao))
  (string= (rest (assoc :result
                        (nao-send-http nao :endpoint "/headtouch/yes_no")))
           "front"))