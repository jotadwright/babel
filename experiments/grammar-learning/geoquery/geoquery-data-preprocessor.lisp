
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
          for test-res = (test-pl-to-preds mr)
          unless test-res
          do (loop for lang-entry in nl
                   for lang = (second (first (second lang-entry)))
                   for value = (cl-json:encode-json-to-string (list (cons "utterance" (third lang-entry))
                                                                             (cons "meaning" (string-replace (format nil "~S" pred-mr) "GL-DATA::" ""))
                                                                             (cons "geo-prolog" mr)
                                                                             (cons "pred-to-pl" (format nil "~S" (predicates-to-geo-prolog pred-mr)))
                                                                             (cons "geo-to-polish" (format nil "~S" (geo-prolog-to-polish-notation mr)))
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
  (loop with stack = (list (geo-prolog-to-polish-notation geo-prolog-string))
        with var-list = (get-all-vars geo-prolog-string)
        with program-level = (first (push (get-next-var-from-var-list var-list) var-list))
        
        while stack     
        for part = (pop stack)
        append (loop with next-level = program-level
                     for predicate in part
                     for term = (when (equal (type-of predicate) 'cons)
                                  (first predicate))
                     for args = (when (equal (type-of predicate) 'cons)
                                  (rest predicate))
                     for vars = (append (list program-level)
                                        (loop for el in args
                                              if (or (numberp el)
                                                     (variable-p el))
                                              collect el
                                              else
                                              collect (setf next-level (get-next-var next-level)))) ;; increment level here
                     for cand-emb-preds = (loop for el in args
                                                unless (or (numberp el)
                                                     (variable-p el))
                                                collect el)
                     for accum-val = (append (list term) vars)
                     do ;(when cand-emb-preds
                        ;  (setf next-level (get-next-var next-level)))
                     (cond (;; predicate is a constant e.g. 'new_york
                               (equal (type-of predicate) 'symbol)
                               (setf accum-val (list predicate program-level))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list))))
                              
                              ;; term is actually a list, continue with the list in next iteration   
                              ((equal (type-of term) 'cons)
                               (setf accum-val nil)
                  
                               (setf stack (append stack (append (list term) cand-emb-preds))))
                              ;; rest is not preceded by round brackets, add them
                              ((and cand-emb-preds
                                    (or
                                     (equal (type-of (car cand-emb-preds)) 'symbol) ;(new_york)
                                     (equal (type-of (caar cand-emb-preds)) 'symbol))) ;((cityid
                               (setf stack (append stack (list cand-emb-preds))))
                              
                              ;; term is an actual term, collect the result and continue with the embedded predicates
                              (t
                               (setf stack (append stack cand-emb-preds))))

                     when accum-val
                     collect accum-val
                     finally (when (and accum-val
                                        (not (equal (type-of predicate) 'symbol)))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list)))))))
        

        

(defun get-next-var-from-var-list (var-list)
  (get-next-var (first (sort var-list #'> :key #'(lambda (x) (position (symbol-name x) gl::+placeholder-vars+ :test #'equal))))))

(defun get-next-var (last-var)
  (intern (nth (+ 1 (position (symbol-name last-var) gl::+placeholder-vars+ :test #'equal)) gl::+placeholder-vars+)))

(defun geo-prolog-to-polish-notation (geo-prolog-string)
  (let* ((geo-prolog-string (cl-ppcre:regex-replace-all " " geo-prolog-string "_"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([a-z]+_?[a-z]+)[\(]" geo-prolog-string "\(\\1 "))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([A-Z])" geo-prolog-string "?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all ",(_)" geo-prolog-string ",?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "'(.*?)'" geo-prolog-string "\\1"))
         (geo-prolog-string (replace-all geo-prolog-string "," " "))
         (geo-prolog-string (string-append "("geo-prolog-string ")")))
    (read-from-string geo-prolog-string)))


(defun predicates-to-geo-prolog (predicates)
  "go through all predicates in reverse, and keep substituting subprograms until no levels are left"
  (loop with stack = (reverse (copy-list predicates))      
        while stack     
        for predicate = (pop stack)
        for term = (first predicate)
        for nxt-lvl-var = (cond ((equal term 'count)
                                 (fourth predicate))
                                ((equal term 'cityid)
                                 (third predicate))
                                (t
                                 (last-elt predicate)))
        for embedded-preds = (find-all nxt-lvl-var (remove predicate predicates) :key #'second :test #'equal)
        for stripped-embedded-preds = (mapcar #'(lambda (predicate) (remove nxt-lvl-var predicate)) embedded-preds)
        for delisted-embedded-preds = (cond ((and
                                              (= (length stripped-embedded-preds) 1)
                                              (= (length (first stripped-embedded-preds)) 1))
                                             (first (first stripped-embedded-preds)))
                                            ((= (length stripped-embedded-preds) 1)
                                             (first stripped-embedded-preds))
                                            (t
                                             stripped-embedded-preds))
        for remaining-preds = (loop for pred in predicates
                                    unless (member pred embedded-preds)
                                    collect pred)
        when embedded-preds
        do (setf predicates (substitute (substitute delisted-embedded-preds nxt-lvl-var predicate) predicate remaining-preds))
        finally (return (remove (second (first predicates)) (first predicates)))))
        
(defun test-pl-to-preds (geo-prolog-string)
  (equal (geo-prolog-to-polish-notation geo-prolog-string)
       (list (predicates-to-geo-prolog (geo-prolog-to-predicates geo-prolog-string)))))

(test-pl-to-preds "answer(A,(size(B,A),const(B,cityid('new york',_))))")
(test-pl-to-preds "answer(A,(state(A),const(B,riverid(chattahoochee)),river(B),traverse(B,A)))")
(test-pl-to-preds "answer(A,count(B,(river(B),not((traverse(B,C),state(C),loc(D,C),capital(D),const(D,cityid(albany,_))))),A))")
(test-pl-to-preds "answer(A,count(B,(state(B),not((loc(C,B),river(C)))),A))")
(test-pl-to-preds "answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,stateid(mississippi))))")
(test-pl-to-preds "answer(A,(highest(A,(place(A),loc(A,B),state(B))),lowest(C,(loc(C,B),place(C))),elevation(C,0)))")
(test-pl-to-preds "answer(A,(population(B,A),const(B,cityid(springfield,_))))")
(test-pl-to-preds "answer(A,(state(A),loc(B,A),city(B),const(B,cityid('salt lake city',_))))")

;;;;;;;;;;;;;; ERRORS in predicates-to-geo-prolog ;;;;;;;;;;;;;;
; "What is the total length of all rivers in the USA ?"
(test-pl-to-preds "answer(A,sum(B,(len(C,B),river(C)),A))")

; "What is the total population of the states that border Texas ?"
(test-pl-to-preds "answer(A,sum(B,(population(C,B),state(C),next_to(D,C),const(D,stateid(texas))),A))")

; "What is the combined population of all 50 states ?"
(test-pl-to-preds "answer(A,sum(B,(population(C,B),state(C)),A))")

; "What is the combined area of all 50 states ?"
(test-pl-to-preds "answer(A,sum(B,(area(C,B),state(C)),A))")

; "What is the area of all the states combined ?"
(test-pl-to-preds "answer(A,sum(B,(area(C,B),state(C)),A))")

(test-pl-to-preds "answer(A,sum(B,(area(C,B),state(C)),A))")

; "What is the population of Washington DC ?"
(geo-prolog-to-predicates "answer(A,(population(B,A),const(B,cityid(washington,dc))))")
(test-pl-to-preds "answer(A,(population(B,A),const(B,cityid(washington,dc))))")

; "What rivers run through Austin Texas ?"
(geo-prolog-to-predicates "answer(A,(river(A),traverse(A,B),const(B,cityid(austin,tx))))")
(test-pl-to-preds "answer(A,(river(A),traverse(A,B),const(B,cityid(austin,tx))))")

;;;;;;;;;;;;;; ERRORS in geo-prolog-to-predicates ;;;;;;;;;;;;;;
;; they are all incremented too much when there are two ids

; "What states in the United States have a city of Springfield ?"
(geo-prolog-to-predicates "answer(A,(state(A),loc(A,B),const(B,countryid(usa)),loc(C,A),const(C,cityid(springfield,_))))")

; "What state borders the least states excluding Alaska and excluding Hawaii ?"
(geo-prolog-to-predicates "answer(A,fewest(A,B,(state(A),next_to(A,B),state(B),not((const(A,stateid(alaska)))),not((const(A,stateid(hawaii)))))))")

; "What is the length of the Colorado river in Texas ?"
(geo-prolog-to-predicates "answer(A,(len(B,A),const(B,riverid(colorado)),river(B),loc(B,C),const(C,stateid(texas))))")

; "How many states in the US does the shortest river run through ?"
(geo-prolog-to-predicates "answer(A,count(B,(state(B),loc(B,C),const(C,countryid(usa)),shortest(D,river(D)),traverse(D,B)),A))")

; "How many states border Colorado and border New Mexico ?"
(geo-prolog-to-predicates "answer(A,count(B,(state(B),next_to(B,C),const(C,stateid(colorado)),next_to(B,D),const(D,stateid('new mexico'))),A))")

; "How many rivers in Texas are longer than the Red ?"
(geo-prolog-to-predicates "answer(A,count(B,(river(B),loc(B,C),const(C,stateid(texas)),longer(B,D),const(D,riverid(red))),A))")

; "How many cities named Austin are there in the USA ?"
(geo-prolog-to-predicates "answer(A,count(B,(city(B),const(B,cityid(austin,_)),loc(B,C),const(C,countryid(usa))),A))")

;(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")


