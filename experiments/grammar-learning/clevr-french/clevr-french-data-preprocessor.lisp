
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


(defparameter *train-file*
  (babel-corpora-pathname '("clevr-grammar-learning" "clevr-french")
                          :file-name "CLEVR_val_questions_rpn_with_irl_program_all"
                          :file-extension "json"))


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
  (let ((stage-1-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" "clevr-french" ,mode-string)
                                                     :file-name "stage-1"
                                                     :file-extension "jsonl")))
        (stage-2-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" "clevr-french" ,mode-string)
                                                     :file-name "stage-2"
                                                     :file-extension "jsonl")))
        (stage-3-file (eval `(babel-corpora-pathname '("clevr-grammar-learning" "clevr-french" ,mode-string)
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
          (with-open-file (stream data-path :direction :input :external-format :utf-8 :element-type :default)
            (loop for line = (read-line stream nil nil)
                  for data = (when line (cl-json:decode-json-from-string line))
                  for meaning-string = (when line (rest (assoc :meaning data)))
                  for meaning = (when line (read-from-string meaning-string))
                  for predicates = (when line (remove 'bind (mapcar #'car meaning)))
                  while line
                  do (cond ((loop for p in predicates
                                  always (find p *stage-1-primitives*))
                            ;; move to stage-1 file
                            (write-line line s1))
                                
                           ((loop for p in predicates
                                  always (find p *stage-2-primitives*))
                            ;; move to stage-2 file
                            (write-line line s2))
                           (t
                            ;; move to stage-3 file
                            (write-line line s3))))))))))

;(main *train-file* :mode-string "val")


