(in-package :robot-interface)

(export '(go-to-posture current-posture
         look-direction nod shake-head
         point celebrate))

;;;; Postures

(defgeneric go-to-posture (robot posture &key speed)
  (:documentation "Make the robot go to a certain posture.
   The available postures depend on the robot type.
   The speed keyword can be used to control the speed of the motors.
   This is a float in [0,1]. This is a blocking call until the
   posture is reached. Returns t when the posture is reached."))

(defgeneric current-posture (robot)
  (:documentation "Get the current posture of the robot"))

;;;; Moving the head

(defgeneric look-direction (robot direction angle &key speed)
  (:documentation "Make the robot look in a given direction (:left, :right, :up, :down),
   specifying the angle with a number in the range [0,1].
   0 will reset the motor to the default position.
   1 will cause maximum movement in that direction.
   Keyword argument can be used to control the speed of the motors.
   This is also a float in [0,1].
   Returns t when the angle is set."))

;;;; Nodding and shaking the head

(defgeneric nod (robot)
  (:documentation "Make the robot nod"))

(defgeneric shake-head (robot)
  (:documentation "Make the robot shake its head"))

;;;; Pointing

(defgeneric point (robot arm)
  (:documentation "Make the robot point with the given arm (:left, :right or :both).
   Makes the robot raise its hand up to shoulder height with a streched arm. This is
   a blocking call. Returns t when the action is complete."))

(defgeneric celebrate (robot)
  (:documentation "Makes the robot raise both arms in celebration! This is a blocking call.
   Returns t when the action is complete."))
