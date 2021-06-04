
(ql:quickload :cl-json)

(defparameter *train-file*
  (parse-namestring "/Users/u0077062/Box/CLEVR_data/CLEVR_train.csv"))

(defparameter *validation-file*
  (parse-namestring "/Users/u0077062/Box/CLEVR_data/CLEVR_val.csv"))

(defparameter *stage-1-file*
  (parse-namestring "/Users/u0077062/Projects/babel-corpora/clevr-grammar-learning/validation/stage-1.txt"))
(defparameter *stage-2-file*
  (parse-namestring "/Users/u0077062/Projects/babel-corpora/clevr-grammar-learning/validation/stage-2.txt"))
(defparameter *stage-3-file*
  (parse-namestring "/Users/u0077062/Projects/babel-corpora/clevr-grammar-learning/validation/stage-3.txt"))


(defparameter *stage-1-primitives*
  '(count! exist filter get-context query unique))
(defparameter *stage-2-primitives*
  '(count! exist filter get-context 
           query relate same unique))
(defparameter *stage-3-primitives*
  '(count! equal-integer less-than greater-than
           equal? exist filter get-context intersect
           query relate same union! unique))

(defun main (data-path)
  (with-open-file (s1 *stage-1-file* :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (with-open-file (s2 *stage-2-file* :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (with-open-file (s3 *stage-3-file* :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (with-open-file (stream data-path :direction :input)
          (read-line stream nil nil) ;; skip header
          (loop for line = (read-line stream nil nil)
                for i from 1
                while line
                do (let* ((fields (split-sequence "," line))
                          (question (second fields))
                          (meaning-string (first (last fields)))
                          (meaning (read-from-string meaning-string))
                          (predicates (remove 'bind (mapcar #'car meaning)))
                          (len (count #\space question))
                          (json (cl-json:encode-json-to-string (list (cons "question" question)
                                                                     (cons "meaning" meaning-string)
                                                                     (cons "len" len)))))
                          
                          
                     (cond ((loop for p in predicates
                                  always (find p *stage-1-primitives*))
                            ;; move to stage-1 file
                            (write-line json s1))
                                
                           ((loop for p in predicates
                                  always (find p *stage-2-primitives*))
                            ;; move to stage-2 file
                            (write-line json s2))
                           (t
                            ;; move to stage-3 file
                            (write-line json s3))))))))))

(main *validation-file*)