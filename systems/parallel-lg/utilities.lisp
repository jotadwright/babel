(in-package :parallel-lg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;; Utility functions for the parallel-lg package. ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-stream (stream)
  "Reads s-expression from stream or returns nil if end-of-stream."
  (read stream nil))

(defun read-line-from-stream (inputstream)
  "Reads string from stream or returns nil if end-of-stream."
  (read-line inputstream nil))

(defun write-to-client-process (client-process string)
  "Writes a command to a client-process."
  (write-line-to-stream (uiop/launch-program:process-info-input (process client-process)) string))

(defun write-line-to-stream (outputstream result)
  "Writes a line to a stream and flushes."
  (format outputstream "~a~%" result)
  (force-output outputstream))

;; For demo purposes.
(defun use-cpu (load)
  "Uses CPU for about load seconds (on i7)."
  (loop for i from 1 upto (* load 1000000000)
        do (* i load)
        finally (return load)))
