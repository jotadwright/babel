(in-package :robot-interface)

(export '(current-posture sit sit-relax stand stand-init stand-zero crouch lie-down-belly lie-down-back
          look-up-down look-left-right
          say-yes say-no
          point))

;;;; Postures

(defun sit (robot)
  "Go to a sitting posture"
  #+nao (nao-go-to-posture robot :sit))

(defun sit-relax (robot)
  #+nao (nao-go-to-posture robot :sit-relax))

(defun stand (robot)
  "Go to a standing posture"
  #+nao (nao-go-to-posture robot :stand))

(defun stand-init (robot)
  "Stand with more balance"
  #+nao (nao-go-to-posture robot :stand-init))

(defun stand-zero (robot)
  "Stand with stretched arms"
  #+nao (nao-go-to-posture robot :stand-zero))

(defun crouch (robot)
  "Go to a crouching posture"
  #+nao (nao-go-to-posture robot :crouch))

(defun lie-down-belly (robot)
  #+nao (nao-go-to-posture robot :belly))

(defun lie-down-back (robot)
  #+nao (nao-go-to-posture robot :back))

(defun current-posture (robot)
  "Return the current posture"
  #+nao (nao-get-posture robot))

;;;; moving the head
(defun look-up-down (robot value &key (unit :degrees))
  "Move the head up or down. Default unit: degrees. Radians also possible."
  #+nao (cond
         ((eq unit :degrees)
          (nao-set-joint robot :head :head-pitch :value (deg-to-rad value)))
         ((eq unit :radians)
          (nao-set-joint robot :head :head-pitch :value value))
         (t
          (error (format nil "~a is not a valid unit" unit)))))

(defun look-left-right (robot value &key (unit :degrees))
  " Move the head left or down. Default unit: degrees. Radians also possible. "
  #+nao (cond
         ((eq unit :degrees)
          (nao-set-joint robot :head :head-yaw :value (deg-to-rad value)))
         ((eq unit :radians)
          (nao-set-joint robot :head :head-yaw :value value))
         (t
          (error (format nil "~a is not a valid unit" unit)))))

(defun say-yes (robot)
  " Say yes using the robot's head "
  #+nao (nao-head-say robot :yes))

(defun say-no (robot)
  " Say no using the robot's head "
  #+nao (nao-head-say robot :no))

(defun point (robot arm)
  "Raise left or right arm."
  #+nao (cond
         ((eql arm :left)
          (nao-raise-arm robot :left))
         ((eql arm :right)
          (nao-raise-arm robot :right))
         ((eql arm :both)
          (nao-raise-arm robot :both))))
