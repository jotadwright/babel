(in-package :ont-alignment)

;----------------------;
;functions to work with;
;---------lists--------;

(defun pick-random-elem (list)
  "function to get a random element from a given list"
  (nth (random (length list)) list))

(defun get-nth-element (lst n)
  "Returns the nth element of list lst"
  (nth n lst))

(defun list-to-string (list)
  "Takes a list as input and makes it a string able to be processed in an in-clause in sql."
  (let ((formatted-string (concatenate 'string "'" (first list) "'")))
    (loop for elem in list
          do (if (not (string= (first list) elem))
               (setf formatted-string (concatenate 'string formatted-string ", '" elem "'"))))
    formatted-string))

(defun list-to-set (lst)
  "Takes a list as entry and returns a set."
  (remove-duplicates lst :test #'equal))

(defun most-frequent-element (lst)
  "Takes a list as a paremeter and returns the most frequent element of that list."
  (let ((counter (make-hash-table)))
    (dolist (item lst)
      (let ((count (gethash item counter 0)))
        (setf (gethash item counter) (1+ count))))
    (let ((max-count 0)
          (max-item nil))
      (maphash (lambda (item count)
                 (when (> count max-count)
                   (setf max-count count
                         max-item item)))
               counter)
      max-item)))

;----------------------;
;functions to work with;
;--------strings-------;

(defun is-substring (substring string)
  "Check if a substring is part of a string."
  (let ((result (search substring string)))
    (if (not (null result))
      (format t "~a is a substring of ~a~%" substring string)
      (format t "~a is not a substring of ~a~%" substring string))))

;-----------------------;
;functions to parse json;
;-----------------------;

(defun read-json-data (json-file)
  "function to read data from json file"
  (let* ((file-stream (open json-file :if-does-not-exist nil))
         (json-data (when file-stream (json:decode-json file-stream))))
    (close file-stream)
    json-data))