(in-package :au-benchmark.base)

(export '(fac!))

(defun fac! (nr &optional (start 1))
  (if (= nr 0)
    1
    (loop for i from start to nr
          for product = i then (* product i)
          finally (return product))))