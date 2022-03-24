(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(defparameter *clevr-data-path*
  (make-pathname :directory '(:absolute "scratch" "brussel" "102" "vsc10279" "CLEVR-v1.0")))

(defun args->plist (args)
  (loop for arg in args
        for i from 0
        if (evenp i) collect (internal-symb (upcase arg))
        else collect arg))

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (print arg-plist)
    (let ((start (* (parse-integer (getf arg-plist 'start)) 100))
          (end (+ 99 (* (parse-integer (getf arg-plist 'end)) 100))))
          (evaluate-clevr-dialogs-symbolic start end))))

(main #+sbcl (rest sb-ext:*posix-argv*))
