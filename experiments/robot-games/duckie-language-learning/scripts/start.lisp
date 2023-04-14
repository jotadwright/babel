(ql:quickload :duckie-language-learning)
(in-package :duckie-language-learning)

(monitors::activate-monitor trace-irl)
(activate-monitor trace-fcg)

;;;; START DEMO

(setf *server-address* "http://192.168.2.5:7000/")

(defparameter *demo*
  (make-instance 'duckie-language-learning-experiment))

(run-interaction *demo*)
