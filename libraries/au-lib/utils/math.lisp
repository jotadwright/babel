(in-package :au-lib)

(export '(fac!
          average))

(defun fac! (nr &optional (start 1))
  (if (= nr 0)
    1
    (loop for i from start to nr
          for product = i then (* product i)
          finally (return product))))

(defun average (list)
  (loop for elm in list
        count t into length
        sum elm into sum
        finally
          (return (if (= length 0)
                    0
                    (coerce (/ sum length) 'float)))))