(in-package :parallel-lg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; This file implements the client process side of the :parallel-lg package  ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *lock-file* nil
  "The name of the temporary lock file.")

(defparameter *output-file* nil
  "The name of the temporary output file.")

(defparameter *process-fn* nil
  "The name of the client process.")

(defparameter *write-fn* nil
  "The name of the client process.")

(defparameter *keep-order* nil
  "Whether order of items in input corpus should be kept.")


;; Initialising the client process ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-asdf-systems (asdf-systems)
  "Loads the required asdf systems"
  (ql:quickload asdf-systems)
  (format nil "Process initialised with asdf-systems: ~a." asdf-systems))

(defun initialise-process (&key lock-file
                                output-file
                                process-fn
                                write-fn
                                keep-order)
  "Initialises the cl-pcp client process by setting the relevant global variables."
  ;; Set global variables
  (setf *lock-file* lock-file
        *output-file* output-file
        *process-fn* process-fn
        *write-fn* write-fn
        *keep-order* keep-order)
  (delete-file *lock-file*)
  (format nil "Process initialised with lock-file: ~s, temporary output-file: ~s, process-fn: ~a, write-fn: ~a and keep-order: ~a."
          lock-file output-file process-fn write-fn keep-order))

(defun run (item-nr input)
  "Runs the client process for input."
  (let* ((processed-input (funcall *process-fn* input)))
    ;; Now write the output
    (with-open-file (stream *output-file* :direction :output :if-does-not-exist :create :if-exists :append)
      (if *keep-order*
        (format stream "~a.~a~%" item-nr (funcall *write-fn* processed-input))
        (format stream "~a~%" (funcall *write-fn* processed-input)))
      (finish-output stream)))
  ;; Finally, unlock the process by deleting the lock file.
  (delete-file *lock-file*)
  (format nil "Item ~a done." item-nr))
