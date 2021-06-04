(ql:quickload :cl-json)


(defun read-data (challenge-file)
  (with-open-file (stream challenge-file)
    (let* ((total-num-words 0)
           (total-meaning-length 0)
           (stage-data (loop for line = (read-line stream nil)
                             for data = (when line (cl-json:decode-json-from-string line))
                             for utterance = (when data (cdr (assoc :question data)))
                             for meaning = (when data (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal))
                             while data
                             do (setf total-num-words (+ total-num-words (length (split-sequence " " utterance))))
                             (setf total-meaning-length (+ total-meaning-length (length meaning)))
                             collect (cons utterance meaning)))
           (avg-utterance-length (float (/ total-num-words (length stage-data))))
           (avg-meaning-length (float (/ total-meaning-length (length stage-data)))))
      (format t "~a~%" challenge-file)
      (format t "avg utterance length (words): ~a~%" avg-utterance-length)
      (format t "avg meaning length (predicates): ~a~%" avg-meaning-length))))

(progn
  (read-data "/Users/u0077062/Projects/babel-corpora/clevr-grammar-learning/train/stage-1.txt")
  (read-data "/Users/u0077062/Projects/babel-corpora/clevr-grammar-learning/validation/stage-1.txt"))