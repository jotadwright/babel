(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(defun main (args)
  (let ((arg-plist (args->plist args)))
    (print arg-plist)
    (let ((start (* (parse-integer (getf arg-plist 'start)) 100))
          (end (+ 99 (* (parse-integer (getf arg-plist 'end)) 100))))
          (evaluate-clevr-dialogs-symbolic start-scene end-scene))))

(main #+sbcl (rest sb-ext:*posix-argv*))