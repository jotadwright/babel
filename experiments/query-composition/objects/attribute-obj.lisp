(in-package :qc)

(defclass attribute ()
  ((name
    :initarg :name
    :type string
    :accessor name)
   (type-att
    :initarg :type-att
    :accessor type-att)))


(defun sort-by-type (lst result)
  (let ((list-of-att '()))
    (dolist (res result)
      (dolist (att lst)
        (if (equal (type-of res) (type-att att))
          (push att list-of-att))))
    list-of-att))
    





