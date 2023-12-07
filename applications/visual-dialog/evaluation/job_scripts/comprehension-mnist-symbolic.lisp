(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(defparameter *mnist-data-path*
  (make-pathname :directory '(:absolute "scratch" "brussel" "101" "vsc10168" "Corpora" "MNIST-Dialog")))

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (print arg-plist)
    (let ((start (* (parse-integer (getf arg-plist 'start)) 100))
          (end (+ 99 (* (parse-integer (getf arg-plist 'end)) 100)))
          (world (make-instance 'world :entries '((:dataset .  :mnist)
                                                  (:datasplit . :val)
                                                  (:mode . :symbolic)))))
      (comprehend-dialogs start end world))))

(main #+sbcl (rest sb-ext:*posix-argv*))
