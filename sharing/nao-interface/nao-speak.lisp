(in-package :nao-interface)

(export '(nao-speak nao-start-speech-recognition nao-stop-speech-recognition))

;; + nao speak +

(defgeneric nao-speak (nao speech &key speed)
  (:documentation "Make the Nao say something"))

(defmethod nao-speak ((nao nao) speech &key (speed 100))
  (assert (stringp speech))
  (assert (and (numberp speed) (<= 0 speed) (>= 100 speed)))
  (let* ((speech (if (/= speed 100)
                   (concatenate 'string (format nil "\\rspd=~a\\" speed) speech)
                   speech))
         (response (nao-send-http nao "/speech/say" :data `((speech . ,speech)))))
    (rest (assoc :success response))))

;; + nao speech recognition +

(defgeneric nao-start-speech-recognition (nao vocabulary)
  (:documentation "Start the speech recognition process, given a certain vocabulary of words"))

(defgeneric nao-stop-speech-recognition (nao subscriber)
  (:documentation "Stop the speech recognition process and return the detected word(s)"))

(defmethod nao-start-speech-recognition ((nao nao) vocabulary)
  (assert (listp vocabulary))
  (assert (apply #'always (mapcar #'stringp vocabulary)))
  (let ((response (nao-send-http nao "/speech/start_recognition"
                                 :data `((dummy . dummy)
                                         (vocabulary . ,vocabulary)))))
    (rest (assoc :subscriber response))))

(defmethod nao-stop-speech-recognition ((nao nao) subscriber)
  (assert (stringp subscriber))
  (let ((response (nao-send-http nao "/speech/stop_recognition"
                                 :data `((subscriber . ,subscriber)))))
    (rest (assoc :recognised response))))

#|
(defmethod nao-speak ((nao nao) speech &key (speed 100))
  (let* ((speech (if (not (= speed 100))
                   (string-append (format nil "\\rspd=~a\\" speed) speech)
                   speech))
         (json (make-json 'speak :data `((speech . ,speech)))))
    (nao-send-http nao json)))

(defmethod nao-start-speech-rec ((nao nao) vocabulary)
  (let* ((json (make-json 'speech-recognition :data `((action . "start")
                                                      (vocabulary . ,vocabulary))))
         (response (nao-send-http nao json)))
    (get-response-key response :key :subscriber)))

(defmethod nao-stop-speech-rec ((nao nao) subscriber)
  (let* ((json (make-json 'speech-recognition :data `((action . "stop")
                                                      (subscriber . ,subscriber))))
         (response (nao-send-http nao json)))
    (get-response-key response :key :detected)))
|#