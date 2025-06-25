(in-package :slp)

(defun test-coverage (dataset grammar)
  (let ((comprehension-failed-list nil)
        (production-failed-list nil)
        (comprehension-succeed-rate nil)
        (production-succeed-rate nil))
    (deactivate-monitor slp::trace-slp)
    (loop for sign-act in (data dataset)
          unless (equivalent-irl-programs?
                  (comprehend
                   (coincides->simultaneous-predicates
                    (form sign-act))
                  :cxn-inventory grammar)
                  (meaning sign-act))
            do (push (id sign-act) comprehension-failed-list)
          unless (equivalent-irl-programs?
                  (predicates
                   (formulate
                    (meaning sign-act)
                    :cxn-inventory grammar))
                  (predicates
                   (coincides->simultaneous-predicates
                    (form sign-act))))
            do (push (id sign-act) production-failed-list))
    (setf comprehension-succeed-rate
          (- 100 (* (/ (length comprehension-failed-list)(length (data dataset))) 100)))
    (setf production-succeed-rate
          (- 100 (* (/ (length production-failed-list)(length (data dataset))) 100)))
    (format t "~%~%++++++++++++++++++++++++++++++~%Grammar coverage~%++++++++++++++++++++++++++++++~%~%comprehension success rate: ~d\%~%production success rate: ~d\%~%~%++++++++++++++++++++++++++++++~%"
            comprehension-succeed-rate
            production-succeed-rate)))