(in-package :robot-interface)

(export '(make-robot make-new-connection disconnect-robot))

(defun make-robot (&key type ip server-port (connect-automatically t))
  "Make a new robot of the type 'type', specifying its
   IP address and a port number."
  (let ((robot-class (find-class type nil)))
    (if robot-class
      (apply #'make-instance robot-class (list :ip ip :server-port server-port :connect-automatically connect-automatically))
      (error "The robot type ~a is not known" type))))

(defgeneric make-new-connection (robot &key test-connection)
  (:documentation "Open the connection to the robot.
   The connection can be tested after startup"))

(defgeneric robot-connected-p (robot)
  (:documentation "Check if the robot is connected"))

(defgeneric disconnect-robot (robot)
  (:documentation "Disconnect the robot"))