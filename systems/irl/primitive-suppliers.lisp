(in-package :irl)

(defclass primitive-supplier-with-simple-queue ()
  ((remaining-primitives
    :type list :initarg :remaining-primitives
    :accessor remaining-primitives))
  (:documentation "A list of primitives that are still to try"))

(defun primitives-for-evaluation (pipn)
  (copy-list
   (if (get-configuration pipn :shuffle-primitives-before-evaluation)
     (shuffle (primitives-remaining pipn))
     (primitives-remaining pipn))))

(defmethod create-primitive-supplier ((node pip-node) (mode (eql :simple-queue)))
  (make-instance 'primitive-supplier-with-simple-queue
                 :remaining-primitives
                 (primitives-for-evaluation node)))

(defmethod next-primitive ((primitive-supplier primitive-supplier-with-simple-queue)
                           (node pip-node))
  (pop (remaining-primitives primitive-supplier)))