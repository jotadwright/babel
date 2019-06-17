(in-package :nao-interface)

(export '(go-to-posture current-posture look-direction nod shake-head point celebrate))

;;;; go to posture
(defmethod go-to-posture :before ((nao nao) posture &key (speed 0.5))
  (assert (numberp speed))
  (assert (and (<= speed 1) (>= speed 0))))

(defmethod go-to-posture ((nao nao) (posture (eql :sit)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "Sit")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :sit-relax)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "SitRelax")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :stand)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "Stand")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :stand-init)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "StandInit")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :stand-zero)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "StandZero")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :crouch)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "Crouch")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :lying-belly)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "LyingBelly")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) (posture (eql :lying-back)) &key (speed 0.5))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/posture/set"
                              :data `((posture . "LyingBack")
                                      (speed . ,speed))))))

(defmethod go-to-posture ((nao nao) posture &key speed)
  (error "The posture ~a is not valid for the Nao" posture))

;;;; current posture
(defmethod current-posture ((nao nao))
  (rest (assoc :posture
               (nao-send-http nao :endpoint "/posture/get"))))

;;;; look direction
(defmethod look-direction :before ((nao nao) direction angle &key (speed 0.5))
  (assert (numberp angle))
  (assert (and (<= angle 1) (>= angle 0)))
  (assert (numberp speed))
  (assert (and (<= speed 1) (>= speed 0))))

(defmethod look-direction ((nao nao) (direction (eql :left)) angle &key (speed 0.5))
  ;; move the head-yaw with a positive angle
  (let* ((max 119.5)
         (value (deg-to-rad (* angle max))))
    (rest (assoc :success
                 (nao-send-http nao :endpoint "/set_joint"
                                :data `((joint . "HeadYaw")
                                        (value . ,value)
                                        (speed . ,speed)))))))

(defmethod look-direction ((nao nao) (direction (eql :right)) angle &key (speed 0.5))
  ;; move the head-yaw with a negative angle
  (let* ((max 119.5)
         (value (- (deg-to-rad (* angle max)))))
    (rest (assoc :success
                 (nao-send-http nao :endpoint "/set_joint"
                                :data `((joint . "HeadYaw")
                                        (value . ,value)
                                        (speed . ,speed)))))))

(defmethod look-direction ((nao nao) (direction (eql :up)) angle &key (speed 0.5))
  ;; move the head-pitch with a negative angle
  (let* ((max 38.5)
         (value (- (deg-to-rad (* angle max)))))
    (rest (assoc :success
                 (nao-send-http nao :endpoint "/set_joint"
                                :data `((joint . "HeadPitch")
                                        (value . ,value)
                                        (speed . ,speed)))))))

(defmethod look-direction ((nao nao) (direction (eql :down)) angle &key (speed 0.5))
  ;; move the head-pitch with a positive angle
  (let* ((max 29.5)
         (value (deg-to-rad (* angle max))))
    (rest (assoc :success
                 (nao-send-http nao :endpoint "/set_joint"
                                :data `((joint . "HeadPitch")
                                        (value . ,value)
                                        (speed . ,speed)))))))

(defmethod look-direction ((nao nao) direction angle &key speed)
  (error "The direction ~a is not valid for the Nao robot" direction))

;;;; nod
(defmethod nod ((nao nao))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/move_head"
                              :data '((yesno . "yes"))))))

;;;; shake head
(defmethod shake-head ((nao nao))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/move_head"
                              :data '((yesno . "no"))))))

;;;; point
(defmethod point ((nao nao) (arm (eql :left)))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/raise_arm"
                              :data '((arm . "Left"))))))

(defmethod point ((nao nao) (arm (eql :right)))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/raise_arm"
                              :data '((arm . "Right"))))))

(defmethod point ((nao nao) (arm (eql :both)))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/raise_arm"
                              :data '((arm . "Both"))))))

(defmethod point ((nao nao) arm)
  (error "The arm ~a is not valid for the Nao robot" arm))

;;;; celebrate
(defmethod celebrate ((nao nao))
  (rest (assoc :success
               (nao-send-http nao :endpoint "/celebrate"))))

