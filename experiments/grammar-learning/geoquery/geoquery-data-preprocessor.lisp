(in-package :fcg)
(ql:quickload :xmls)

(defun parse-geoquery (file-string)
  (with-open-file (stream file-string :direction :input)
    (loop for example in (subseq (xmls:parse-to-list stream) 2 5)
          for id = (second (first (second example)))
          for nl = (find-all "nl" (subseq example 1) :key #'first :test #'equal)
          for mr = (third (find "geo-prolog" (subseq example 1) :key #'(lambda (el) (second (first (second el)))) :test #'equal))
          collect (list id nl mr))))
                   
(defun geo-prolog-to-predicate-semantics (geo-prolog-string)
  (split-string geo-prolog-string '(#\( ))
  )


(geo-prolog-to-predicate-semantics "answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,stateid(mississippi))))")

#|
 ((answer ?a)
  (high-point ?b ?a)
  (state ?b)
  (next-to ?b ?c)
  (const ?c "stateid-mississippi"))
  
    
|#
(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")

;; split term, if var, add var, if term recurse