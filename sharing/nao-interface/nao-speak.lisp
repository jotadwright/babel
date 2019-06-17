(in-package :nao-interface)

(export '(speak observe-word))

;;;; speak
(defmethod speak :before ((nao nao) (utterance string) &key (speed 100))
  (assert (stringp utterance))
  (assert (and (numberp speed) (<= 0 speed) (>= 100 speed))))

(defmethod speak ((nao nao) (utterance string) &key (speed 100))
  (let ((speech (if (/= speed 100)
                  (concatenate 'string (format nil "\\rspd=~a\\" speed) utterance)
                  utterance)))
    (rest (assoc :success
                 (nao-send-http nao :endpoint "/speech/say"
                                :data `((speech . ,utterance)))))))

;;;; listen
(defgeneric nao-start-speech-recognition (nao vocabulary)
  (:documentation "Start the speech recognition process, given a certain vocabulary of words"))

(defgeneric nao-stop-speech-recognition (nao subscriber)
  (:documentation "Stop the speech recognition process and return the detected word(s)"))

(defmethod nao-start-speech-recognition ((nao nao) vocabulary)
  (rest (assoc :subscriber
               (nao-send-http nao :endpoint "/speech/start_recognition"
                              :data `((vocabulary . ,vocabulary))))))

(defmethod nao-stop-speech-recognition ((nao nao) subscriber)
  (rest (assoc :recognised
               (nao-send-http nao :endpoint "/speech/stop_recognition"
                              :data `((subscriber . ,subscriber))))))

(defmethod observe-word :before ((nao nao) (vocabulary list))
  (assert (apply #'always (mapcar #'stringp vocabulary))))

(defmethod observe-word ((nao nao) (vocabulary list))
  (let ((subscriber (nao-start-speech-recognition nao vocabulary)))
    (when (detect-head-touch nao :middle)
      (nao-stop-speech-recognition))))