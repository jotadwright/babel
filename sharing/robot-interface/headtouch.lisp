(in-package :robot-interface)

(export '(detect-head-touch yes-no-feedback))

(defgeneric detect-head-touch (robot sensor)
  ;; this could use a timeout argument
  (:documentation "Detect if the given sensor (:front, :middle or :rear) is touched.
   This is a blocking call. Returns t when touch is detected."))

(defgeneric yes-no-feedback (robot)
  ;; this could use a timeout argument
  (:documentation "Give yes/no feedback to the robot by pressing the front/rear sensor.
   This is a blocking call. When touching front sensor, returns t. Otherwise nil."))