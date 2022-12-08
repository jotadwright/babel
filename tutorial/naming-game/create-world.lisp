(in-package :naming-game)

(defmethod make-world ((experiment experiment))
  "Creates the different objects in the world of experiment"
  (let ((objects (loop for i from 1 to 10
                       collect (read-from-string (format nil "obj-~d" i)))))
    (setf (world experiment) objects)))
