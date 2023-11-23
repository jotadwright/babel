(ql:quickload :grammar-learning)
(in-package :grammar-learning)

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

(defun load-data (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect (cons (cdr (assoc :utterance data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal)))))

(defparameter *data* (load-data (merge-pathnames
                                 (make-pathname :name "geoquery_en" :type "jsonl")
                                 (merge-pathnames
                                  (make-pathname :directory '(:relative "geoquery"))
                                  cl-user:*babel-corpora*))))


(defun generate-consts (lang)
  (with-open-file (f (eval `(babel-corpora-pathname '("geoquery")
                                                    :file-name ,(string-append "lexicon_geoquery_" lang) 
                                                    :file-extension "jsonl"))
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (loop with unique-cxns
        for entry in *data*
        for form = (first entry)
        for meaning = (cdr entry)
        for consts = (find-all 'CONST meaning :test #'string= :key #'first)
        for names = (remove-if-not #'upper-case-p (rest (split-string form " ")) :key #'(lambda (s) (char s 0)))
        when consts
        do (loop 
                 for const in consts
                 for type-id = (find (last-elt const) meaning :test #'string= :key #'second)
                 for terminal = (find (third type-id) meaning :test #'string= :key #'second)
                 for second-terminal = (find (fourth type-id) meaning :test #'string= :key #'second)
                 for lex-meaning = (if second-terminal
                                     (list const type-id terminal second-terminal)
                                     (list const type-id terminal))
                 for value = (cl-json:encode-json-to-string (list (cons "utterance" (format nil " ~{~a ~}" names))
                                                                  (cons "meaning" (format nil "~S" lex-meaning))))                                                    
                 when (and names
                           (< (length names) 3))               
                 do (pushnew value unique-cxns :test #'equal))
        finally (dolist (entry (reverse unique-cxns))
                               (write-line entry f)))))







;(generate-consts "en")
