
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


(defun get-all-vars (geo-prolog-string)
  (loop for raw-el in (remove "" (cl-ppcre::split "[,\(\)]" geo-prolog-string) :test #'equal)
        for el = (string-trim " " raw-el)
        when (upper-case-p (first (coerce el 'list)))
        collect (variablify el)))
  

                   
(defun geo-prolog-to-predicates (geo-prolog-string)
  (loop with curr-predicate = nil
        with result = nil
        with const-found = nil
        with const-term = nil
        with const-var = nil
        with vars = (get-all-vars geo-prolog-string)
        with second-city-id-el = nil
        with city-id-found = nil
        with geo-prolog-string = (replace-all geo-prolog-string "not((" "not_")
        for raw-el in (remove "" (cl-ppcre::split "[,\(\)]" geo-prolog-string) :test #'equal)
        for el = (replace-all (string-trim " " (replace-all raw-el "'" "")) " " "-")
        do (cond (;; we know the const term, skolemnise the city/country name as a new predicate
                  ;; (stateid ?b ?c)(arkansas ?c)
                  (and const-term (not second-city-id-el))
                  (if (equal const-term "cityid")
                    (progn
                      (setf city-id-found t)
                      (setf result (append result (list (list (intern (string-upcase const-term) :GL-DATA)
                                                            const-var
                                                            (get-next-var-from-var-list vars)
                                                            (get-next-var (get-next-var-from-var-list vars))
                                                            )
                                                        (list (intern (string-upcase el) :GL-DATA)
                                                              (get-next-var-from-var-list vars))
                                                             ))) 
                      )
                    (progn (setf result (append result (list (list (intern (string-upcase const-term) :GL-DATA)
                                                            const-var
                                                            (get-next-var-from-var-list vars)
                                                            ))))
                      (setf curr-predicate (list (intern (string-upcase el) :GL-DATA)
                                                 (get-next-var-from-var-list vars)))
                      ))
                  (setf const-term nil))
                 
                 ;; append the last city identifier
                 (city-id-found
                  (setf curr-predicate (list (intern (string-upcase el) :GL-DATA)
                                             (get-next-var (get-next-var-from-var-list vars))))
                   (setf city-id-found nil)
                   )
                 
                 ;; we've passed a const term, save the next term
                 ((and const-found
                       (lower-case-p (first (coerce el 'list))))
                  (setf const-found nil)
                  (setf const-term (string-downcase el)))

                 ;; we found the const var
                 ((and const-found
                       (upper-case-p (first (coerce el 'list))))
                  (setf const-var (variablify el)))
                 
                 ;; it's a term, append the last term to result, start a new one
                 ((lower-case-p (first (coerce el 'list)))
                  (when curr-predicate
                    (setf result (append result (list curr-predicate))))
                  (if (string= el "const")
                    (setf const-found t)
                    (setf curr-predicate (list (intern (string-upcase el) :GL-DATA)))))
                 
                 ;; it's a number
                 ((numberp (read-from-string el))
                  
                  (setf curr-predicate (append curr-predicate (list (read-from-string el))))
                  )
                 ;; it's a variable
                 (t
                  (setf curr-predicate (append curr-predicate (list (variablify el))))
                  (push (variablify el) vars)))
        finally (return (append result (list curr-predicate)))))
        

(defun get-next-var-from-var-list (var-list)
  (get-next-var (first (sort var-list #'> :key #'(lambda (x) (position (symbol-name x) gl::+placeholder-vars+ :test #'equal))))))

(defun get-next-var (last-var)
  (intern (nth (+ 1 (position (symbol-name last-var) gl::+placeholder-vars+ :test #'equal)) gl::+placeholder-vars+)))

(defun geo-prolog-to-polish-notation (geo-prolog-string)
  (let* ((geo-prolog-string (cl-ppcre:regex-replace-all "([a-z]+)[\(]" geo-prolog-string "\(\\1 "))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([A-Z_])" geo-prolog-string "?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "'([a-z]+) ([a-z]+)'" geo-prolog-string "\\1_\\2"))
         (geo-prolog-string (replace-all geo-prolog-string "," " "))
         (geo-prolog-string (string-append "("geo-prolog-string ")")))
    (read-from-string geo-prolog-string)))

(defun serialize-embedded-predicates (geo-prolog-string)
  (remove nil (loop with emb-preds = (geo-prolog-to-polish-notation geo-prolog-string)
                    with var-list = (get-all-vars geo-prolog-string)
                    with program-level = (first (push (get-next-var-from-var-list var-list) var-list))
                    while emb-preds
                    append (loop
                            with next-level = (get-next-var-from-var-list var-list)
                            for predicate in emb-preds
                            for term = (when (equal (type-of predicate) 'cons)
                                         (first predicate))
                            for args = (when (equal (type-of predicate) 'cons)
                                         (rest predicate))
                            for vars = (append (list program-level)
                                               (loop for el in args
                                                     if (variable-p el)
                                                     collect el
                                                     else
                                                     collect next-level))
                            for cand-emb-preds = (loop for el in args
                                                       unless (variable-p el)
                                                       collect el)
                            for accum-val = (append (list term) vars)
                            do (cond (;; predicate is a constant e.g. 'new_york
                                      (equal (type-of predicate) 'symbol)
                                      (setf accum-val (list program-level predicate))
                                      (setf emb-preds nil))
                                      ;; term is actually a list, continue with the list in next iteration   
                                     ((equal (type-of term) 'cons)
                                      (setf accum-val nil)
                                      (setf emb-preds (append (list term) cand-emb-preds)))
                                     ;; term is an actual term, collect the result and continue with the embedded predicates
                                     (t
                                      (setf emb-preds cand-emb-preds)
                                      ))
                            when accum-val
                            collect accum-val
                            ;; update the program-level
                            finally (when accum-val (setf program-level (first (push (get-next-var-from-var-list var-list) var-list))))))))
  
          
          

;; How many rivers do not traverse the state with the capital Albany ?
;(geo-prolog-to-predicates "answer(A,count(B,(river(B),not((traverse(B,C),state(C),loc(D,C),capital(D),const(D,cityid(albany,_))))),A))")

(serialize-embedded-predicates "answer(A,count(B,(river(B),not((traverse(B,C),state(C),loc(D,C),capital(D),const(D,cityid(albany,_))))),A))")

(serialize-embedded-predicates "answer(A,(size(B,A),const(B,cityid('new york',_))))")
#|
(answer ?x ?a ?y)
(size ?y ?b ?a)
(const ?y ?b ?z)
(cityid ?z 'new_york ?_)

|#


(serialize-embedded-predicates "answer(A,count(B,(state(B),not((loc(C,B),river(C)))),A))")
 #|
(answer ?d ?a ?e)
(count ?e ?b ?f ?a)
(state ?f ?b)
(not ?f ?g)
(loc ?g ?c ?b)
(river ?g ?c)

    
|# 

 
;(geo-prolog-to-predicates "answer(A,(size(B,A),const(B,cityid('new york',_))))")

;(geo-prolog-to-predicates "answer(A,(population(B,A),const(B,cityid(springfield,_))))")

;(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")


