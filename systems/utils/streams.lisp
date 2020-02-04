(in-package :utils)

(export '(stream->list))

(defun stream->list (stream &key number-of-lines)
  "Returns a list of the next number-of-lines lines of the stream.
   If number-of-lines is unspecified, the entire stream is processed
   untill an empty line is encountered."
  (if number-of-lines
    (loop repeat number-of-lines
          for line = (read-line stream nil nil)
          when line collect line)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))
