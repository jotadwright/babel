
(ql:quickload :grammar-learning)
(in-package :grammar-learning)


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


(defun get-consts ()
  (loop for entry in *data*
        for form = (first entry)
        for meaning = (cdr entry)
        for consts = (find-all 'CONST meaning :test #'string= :key #'first)
        for names = (remove-if-not #'upper-case-p (rest (split-string form " ")) :key #'(lambda (s) (char s 0)))
        when consts
        do (loop for const in consts
                 for type-id = (find (last-elt const) meaning :test #'string= :key #'second)
                 for terminal = (find (third type-id) meaning :test #'string= :key #'second)
                 for meaning = (list const type-id terminal)
                 do (format t " ~{~a ~} ~a~%" names meaning))))

(get-consts)

(char "bla" 0)