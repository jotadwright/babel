
(ql:quickload :cl-json)


(defun merge-paths-recursively (path-list)
  (cond
   ((null path-list) (make-pathname :directory '(:relative ".")))
   ((atom path-list) (eval `(make-pathname :directory '(:relative ,path-list))))
   (t (merge-pathnames (merge-paths-recursively (cdr path-list))
                       (merge-paths-recursively (car path-list))))))

(defun babel-corpora-pathname (sub-path-list &key file-name file-extension)
  (let ((dir-name (merge-pathnames
                   (merge-paths-recursively sub-path-list)
                   cl-user:*babel-corpora*)))
    
    (if (and file-name file-extension)
      (merge-pathnames (make-pathname :name file-name :type file-extension) dir-name)                  
      dir-name)))

(babel-corpora-pathname '("seq2seq" "comprehension-and-formulation-training-data")
                        :file-name "CLEVR_train_corpus_comprehension"
                        :file-extension "csv")

(defparameter *train-file*
  (babel-corpora-pathname '("seq2seq" "comprehension-and-formulation-training-data")
                          :file-name "CLEVR_train_corpus_comprehension"
                          :file-extension "csv"))

(defparameter *validation-file*
  (babel-corpora-pathname '("seq2seq" "comprehension-and-formulation-training-data")
                          :file-name "CLEVR_val_corpus_comprehension"
                          :file-extension "csv"))
(defparameter *test-file*
  (babel-corpora-pathname '("seq2seq" "comprehension-and-formulation-training-data")
                          :file-name "CLEVR_test_corpus_comprehension"
                          :file-extension "csv"))


(defparameter *stage-1-primitives*
  '(count! exist filter get-context query unique))
(defparameter *stage-2-primitives*
  '(count! exist filter get-context 
           query relate same unique))
(defparameter *stage-3-primitives*
  '(count! equal-integer less-than greater-than
           equal? exist filter get-context intersect
           query relate same union! unique))

(defun main (data-path &key mode-string)
  (let ((stage-1-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" ,mode-string)
                          :file-name "stage-1"
                          :file-extension "jsonl")))
        (stage-2-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" ,mode-string)
                          :file-name "stage-2"
                          :file-extension "jsonl")))
        (stage-3-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" ,mode-string)
                          :file-name "stage-3"
                          :file-extension "jsonl"))))
  (with-open-file (s1 stage-1-file :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (with-open-file (s2 stage-2-file :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (with-open-file (s3 stage-3-file :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (with-open-file (stream data-path :direction :input)
          (read-line stream nil nil) ;; skip header
          (loop for line = (read-line stream nil nil)
                for i from 1
                while line
                do (let* ((fields (split-sequence "," line))
                          (question (second fields))
                          (meaning-string (third fields))
                          (meaning (read-from-string meaning-string))
                          (predicates (remove 'bind (mapcar #'car meaning)))
                          (len (count #\space question))
                          (json (cl-json:encode-json-to-string (list (cons "utterance" question)
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
                            (write-line json s3)))))))))))

(main *train-file* :mode-string "train")
(main *validation-file* :mode-string "val")
(main *test-file* :mode-string "test")