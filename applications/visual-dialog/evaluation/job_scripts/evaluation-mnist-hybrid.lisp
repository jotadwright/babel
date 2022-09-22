(ql:quickload :visual-dialog)
(in-package :visual-dialog)

(print "we zijn in lisp")
(defparameter *mnist-data-path*
  (merge-pathnames
	(make-pathname :directory '(:relative "mnist-dialog"))
	cl-user::*babel-corpora*))
(print *mnist-data-path*)

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
	  (server (format nil "http://127.0.0.1:~a/" (getf arg-plist 'port))))	
	(print server)
        (evaluate-mnist-dialogs-hybrid start end server))))

(main #+sbcl (rest sb-ext:*posix-argv*))
