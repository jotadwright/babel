
(defparameter *input-file*
  (parse-namestring "/Users/jensnevens/Projects/CLEVR_val_enhanced.txt"))
(defparameter *stage-1-dir*
  (parse-namestring "/Users/jensnevens/Projects/clevr-learning-data/stage-1/"))
(defparameter *stage-2-dir*
  (parse-namestring "/Users/jensnevens/Projects/clevr-learning-data/stage-2/"))
(defparameter *stage-3-dir*
  (parse-namestring "/Users/jensnevens/Projects/clevr-learning-data/stage-3/"))


(defparameter *stage-1-primitives*
  '(count! exist filter get-context query unique))
(defparameter *stage-2-primitives*
  '(count! exist filter get-context 
           query relate same unique))
(defparameter *stage-3-primitives*
  '(count! equal-integer less-than greater-than
           equal? exist filter get-context intersect
           query relate same union! unique))


(defun main ()
  (with-open-file (stream *input-file* :direction :input)
    (loop for line = (read-line stream nil nil)
          for i from 1
          while line
          do (let* ((parsed-line (cl-json:decode-json-from-string line))
                    (question (rest (assoc :question parsed-line)))
                    (meaning (read-from-string
                              (rest (assoc :meaning parsed-line))))
                    (predicates (remove 'bind (mapcar #'car meaning)))
                    (len (count #\space question))
                    (filename (format nil "question_~6,'0d_len_~3,'0d" i len)))
               (cond ((loop for p in predicates
                            always (find p *stage-1-primitives*))
                      ;; move to stage-1 folder
                      (let ((new-file
                             (merge-pathnames
                              (make-pathname :name filename :type "txt")
                              *stage-1-dir*)))
                      (ensure-directories-exist new-file)
                      (with-open-file (out new-file)
                        (write-line line out)
                        (force-output out))))
                     ((loop for p in predicates
                            always (find p *stage-2-primitives*))
                      ;; move to stage-2 folder
                      (let ((new-file
                             (merge-pathnames
                              (make-pathname :name filename :type "txt")
                              *stage-2-dir*)))
                        (ensure-directories-exist new-file)
                        (with-open-file (out new-file)
                          (write-line line out)
                          (force-output out))))
                     (t
                      ;; move to stage-3 folder
                      (let ((new-file
                             (merge-pathnames
                              (make-pathname :name filename :type "txt")
                              *stage-3-dir*)))
                        (ensure-directories-exist new-file)
                        (with-open-file (out new-file)
                          (write-line line out)
                          (force-output out)))))))))