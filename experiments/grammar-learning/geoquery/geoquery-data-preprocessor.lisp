
(ql:quickload :xmls)
(ql:quickload :cl-ppcre)
(ql:quickload :grammar-learning)
(in-package :fcg)

(defun parse-geoquery (file-string)
  (with-open-file (stream file-string :direction :input)
    (loop for example in (subseq (xmls:parse-to-list stream) 16 17)
          for id = (second (first (second example)))
          for nl = (find-all "nl" (subseq example 1) :key #'first :test #'equal)
          for mr = (third (find "geo-prolog" (subseq example 1) :key #'(lambda (el) (second (first (second el)))) :test #'equal))
          for pred-mr = (geo-prolog-to-predicates mr)
          collect (list id nl mr pred-mr))))
                   
(defun geo-prolog-to-predicates (geo-prolog-string)
  (loop with curr-predicate = nil
        with result = nil
        with const-found = nil
        with const-term = nil
        for el in (remove "" (cl-ppcre::split "[,\(\)]" geo-prolog-string) :test #'equal)
        do (cond (const-term
                  (setf curr-predicate (append curr-predicate (list (intern (string-upcase (string-append
                                                                                            "+"
                                                                                            const-term
                                                                                            "-"
                                                                                            (string-replace (string-replace el " " "-") #\' "")
                                                                                                            
                                                                                            "+"
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


(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")







(geo-prolog-to-predicates "answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,stateid('mississippi'))))")

#|
 ((answer ?a)
  (high-point ?b ?a)
  (state ?b)
  (next-to ?b ?c)
  (const ?c +stateid-mississippi+))
  
    
|#


;; split term, if var, add var, if term recurse