
(ql:quickload :xmls)
(ql:quickload :cl-ppcre)
(ql:quickload :grammar-learning)
(in-package :fcg)

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

(defun parse-geoquery (file-string)
  (with-open-file (stream file-string :direction :input)
    (loop with lang-hash = (make-hash-table :test #'equal)
          for example in (subseq (xmls:parse-to-list stream) 2)
          for id = (second (first (second example)))
          for nl = (find-all "nl" (subseq example 1) :key #'first :test #'equal)
          for mr = (third (find "geo-prolog" (subseq example 1) :key #'(lambda (el) (second (first (second el)))) :test #'equal))
          for pred-mr = (geo-prolog-to-predicates mr)
          do (loop for lang-entry in nl
                   for lang = (second (first (second lang-entry)))
                   for len = (length pred-mr)
                   for value = (cl-json:encode-json-to-string (list (cons "utterance" (third lang-entry))
                                                                             (cons "meaning" (string-replace (format nil "~S" pred-mr) "GL-DATA::" ""))
                                                                             (cons "len" len)
                                                                             (cons "id" id)
                                                                             (cons "lang" lang)))
                   do (push value (gethash lang lang-hash nil)))
          finally do (loop for lang being the hash-keys in lang-hash using (hash-value data)
                        for data-file = (eval `(babel-corpora-pathname '("geoquery")
                                                                       :file-name ,(string-append "geoquery_" lang) 
                          :file-extension "jsonl"))
                        do (with-open-file (f data-file :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                             (dolist (entry (reverse data))
                               (write-line entry f)))))))
;(format t "~a => ~a~%" lang data)))))
          
      
                        
                   
(defun geo-prolog-to-predicates (geo-prolog-string)
  (loop with curr-predicate = nil
        with result = nil
        with const-found = nil
        with const-term = nil
        for raw-el in (remove "" (cl-ppcre::split "[,\(\)]" (string-replace geo-prolog-string ",_" "")) :test #'equal)
        for el = (string-trim " " raw-el)
        do (cond (const-term
                  (setf curr-predicate (append curr-predicate (list (intern (string-upcase (string-append
                                                                                           
                                                                                            const-term
                                                                                            "-"
                                                                                            (string-replace (string-replace el " " "-") #\' "")
                                                                                                            
                                                                                            
                                                                                            )) :GL-DATA))))
                  (setf const-term nil))
                  

                 ((and const-found
                       (lower-case-p (first (coerce el 'list))))
                  (setf const-found nil)
                  (setf const-term (string-downcase el)))

                 ((lower-case-p (first (coerce el 'list)))
                  (when (string= el "const")
                    (setf const-found t))
                  (when curr-predicate
                    (setf result (append result (list curr-predicate))))
                  (setf curr-predicate (list (intern (string-upcase el) :GL-DATA))))
                 
                 (t
                  (setf curr-predicate (append curr-predicate (list (variablify el))))))
        finally (return (append result (list curr-predicate)))))


;(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")







;(geo-prolog-to-predicates "answer(A,(population(B,A),const(B,cityid(springfield,sd))))")

#|
(answer ?a)
(population ?b ?a)
(cityid ?b ?c ?d)
(springfield ?c)
(sd ?d))


 
 ((answer ?a)
  (high-point ?b ?a)
  (state ?b)
  (next-to ?b ?c)
  (const ?c +stateid-mississippi+))
  
    
|#


;; split term, if var, add var, if term recurse