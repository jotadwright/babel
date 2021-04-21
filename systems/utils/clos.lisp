(in-package :utils)

(export '(set-clos-slot get-clos-slot))

(defun set-clos-slot (instance slot value)
  "Set a clos-slot of an instance of a certain class to a given value"
  #+lispworks (setf (c2mop:slot-value-using-class (find-class (type-of instance))
                                     instance
                                     slot)
                    value)
  #+ccl (setf (slot-value instance slot) value))

(defun get-clos-slot (instance slot)
  "Get the clos-slot of an instance of a certain class"
  (when  (and (slot-exists-p instance slot)
              (slot-boundp instance slot))
    #+lispworks (c2mop:slot-value-using-class (find-class (type-of instance))
                                              instance
                                              slot)
    #+ccl (slot-value instance slot)))


(defun class-all-subclasses (class)
  "Returns all subclasses of a class (recursively), including class"
  (let ((direct-subclasses (c2mop:class-direct-subclasses class)))
    (cond ((null direct-subclasses)
           (list class))
          (t
           (cons class
                 (loop for subclass in direct-subclasses
                       append (class-all-subclasses subclass)))))))



