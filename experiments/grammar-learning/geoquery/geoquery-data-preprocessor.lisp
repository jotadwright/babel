
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
                   for value = (cl-json:encode-json-to-string (list (cons "utterance" (third lang-entry))
                                                                             (cons "meaning" (string-replace (format nil "~S" pred-mr) "GL-DATA::" ""))
                                                                             (cons "geo-prolog" mr)
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

(defun test-geoquery (file-string)
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
                                                                       :file-name ,(string-append "test_geoquery_" lang) 
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

#|(defun atomic-predicate-p (predicate)
  (equal (type-of (car predicate)) 'symbol))

(defun serialise-predicates (predicates next-level)
  (loop with de-listed-predicates = (if (equal (type-of (caar predicates)) 'cons)
                                      (first predicates)
                                      predicates)
        for predicate in de-listed-predicates
        for term = (list (first predicate) next-level)
        for deserialised-predicate = (append term (loop for el in (rest predicate)
                                                        if (or (numberp el)
                                                               (variable-p el))
                                                        collect el
                                                        else
                                                        collect (setf next-level (get-next-var next-level))))
        for embedded-predicates = (loop for el in (rest predicate)
                                        unless (or (numberp el)
                                                   (variable-p el))
                                        collect el)
        if (atomic-predicate-p embedded-predicates)
        append (append (list deserialised-predicate) (loop for var in (rest (rest deserialised-predicate))
                                                               for symb in (remove-if #'variable-p embedded-predicates)
                                                               collect (list symb var)))
        else
        append (append (list deserialised-predicate) (serialise-predicates embedded-predicates next-level))
        ))


(defun geo-prolog-to-predicates (geo-prolog-string)
  "convert geo-prolog to predicate notation, see https://www.cs.utexas.edu/~ml/wasp/geo-funql.html for specification"
 (let* ((polish-predicates (geo-prolog-to-polish-notation geo-prolog-string))
        (var-list (get-all-vars geo-prolog-string))
        (program-level (first (push (get-next-var-from-var-list var-list) var-list))))
   (serialise-predicates polish-predicates program-level)
  ))
|#

(defun geo-prolog-to-predicates (geo-prolog-string)
  "convert geo-prolog to predicate notation, see https://www.cs.utexas.edu/~ml/wasp/geo-funql.html for specification"
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
                     for accum-val = (list (append (list term) vars))
                     do (cond (;; handle double embedding in list
                               (and (equal term 'not)
                                    (equal (type-of (car cand-emb-preds)) 'cons)
                                    (equal (type-of (caar cand-emb-preds)) 'cons)
                                    (equal (type-of (caaar cand-emb-preds)) 'symbol)
                                    (member (caaar cand-emb-preds) '(const)))
                               ;; todo: handle recursively by pushing to front of stack instead - requires solution for level vars which are now correct because of BF queuing
                               
                               
                               ;; create vars for all embedded predicates
                               (setf accum-val (append accum-val (list (append (list (caaar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdaar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                               ;; todo: make a function out of the above to avoid duplication, loop until atom is reached
                               (setf cand-emb-preds (cddaar cand-emb-preds))
                                                         
                               (setf accum-val (append accum-val (list (append (list (caar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                                                                       
                                                         
                               ;; link all embedded predicates with their new level vars                
                               (setf accum-val (append accum-val (loop for var in (rest (rest (last-elt accum-val)))
                                                       for symb in (remove-if #'variable-p (cdar cand-emb-preds))
                                                       collect (list symb var)))))

                         (;; handle cityid, stateid, countryid, placeid, riverid, river
                               (and (equal (type-of (car cand-emb-preds)) 'cons)
                                    (equal (type-of (caar cand-emb-preds)) 'symbol)
                                    (member (caar cand-emb-preds) '(cityid stateid countryid placeid riverid river const)))
                               ;; create vars for all embedded predicates
                               (setf accum-val (append accum-val (list (append (list (caar cand-emb-preds)
                                                                                     (last-elt (last-elt accum-val)))
                                                                               (loop for el in (cdar cand-emb-preds)
                                                                                     if (or (numberp el)
                                                                                            (variable-p el))
                                                                                     collect el
                                                                                     else
                                                                                     collect (setf next-level (get-next-var next-level)))))))
                                                                       
                                                         
                               ;; link all embedded predicates with their new level vars                
                               (setf accum-val (append accum-val (loop for var in (rest (rest (last-elt accum-val)))
                                                       for symb in (remove-if #'variable-p (cdar cand-emb-preds))
                                                       collect (list symb var)))))                               
                              (;; predicate is a constant e.g. 'new_york
                               (equal (type-of predicate) 'symbol)
                               (setf accum-val (list (list predicate program-level)))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list))))
                              
                              ;; term is actually a list, continue with the list in next iteration   
                              ((equal (type-of term) 'cons)
                               (setf accum-val nil)
                  
                               (setf stack (append stack (append (list term) cand-emb-preds))))
                              ;; rest is not preceded by round brackets, add them
                              ((and cand-emb-preds
                                    (or
                                     (equal (type-of (car cand-emb-preds)) 'symbol) ;(new_york)
                                     (equal (type-of (caar cand-emb-preds)) 'symbol))) ;((cityid or ((fewest
                               (setf stack (append stack (list cand-emb-preds))))
                              
                              ;; term is an actual term, collect the result and continue with the embedded predicates
                              (t
                               (setf stack (append stack cand-emb-preds))))

                     when accum-val
                     append accum-val
                     finally (when (and accum-val
                                        (not (equal (type-of predicate) 'symbol)))
                               (setf program-level (first (push (get-next-var-from-var-list var-list) var-list)))))))

(defun get-next-var-from-var-list (var-list)
  (get-next-var (first (sort var-list #'> :key #'(lambda (x) (position (symbol-name x) gl::+placeholder-vars+ :test #'equal))))))

(defun get-next-var (last-var)
  (intern (nth (+ 1 (position (symbol-name last-var) gl::+placeholder-vars+ :test #'equal)) gl::+placeholder-vars+)))

(defun geo-prolog-to-polish-notation (geo-prolog-string)
  (let* ((geo-prolog-string (cl-ppcre:regex-replace-all ", " geo-prolog-string ","))
         (geo-prolog-string (cl-ppcre:regex-replace-all " " geo-prolog-string "_"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([a-z]+_?[a-z]+)[\(]" geo-prolog-string "\(\\1 "))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([A-Z])" geo-prolog-string "?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all ",(_)" geo-prolog-string ",?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "'(.*?)'" geo-prolog-string "\\1"))
         (geo-prolog-string (replace-all geo-prolog-string "," " "))
         (geo-prolog-string (string-append "("geo-prolog-string ")")))
    (read-from-string geo-prolog-string)))

(defun strip-superfluous-brackets (predicates)
  (cond ((and
          (= (length predicates) 1)
          (equal (type-of (first predicates)) 'list)
          (= (length (first predicates)) 1))
         (first (first predicates)))
        ((= (length predicates) 1)
         (first predicates))
        (t
         predicates)))

(defun predicates-to-geo-prolog (predicates)
  "go through all predicates in reverse, and keep substituting subprograms until no levels are left"
  (loop with stack = (reverse (copy-list predicates))      
        while stack     
        for predicate = (pop stack)
        for term = (first predicate)
        for nxt-lvl-vars = (cond ((member term '(count sum))
                                  (list (fourth predicate)))
                                 ((equal term 'fewest)
                                  (list (fifth predicate)))
                                 ((and (equal term 'cityid)
                                       (equal (fourth predicate) '?_))
                                  (list (third predicate)))
                                 ((equal term 'cityid)
                                  (list (third predicate) (fourth predicate)))
                                 (t
                                  (last predicate)))
        ;; find all embeddable predicates for a certain level
        for embedded-preds = (loop for nxt-lvl-var in nxt-lvl-vars
                                   for em-pred = (find-all nxt-lvl-var (remove predicate predicates) :key #'second :test #'equal)
                                   when em-pred
                                   append em-pred)
        ;; remove the lvl variable from the predicates
        for stripped-embedded-preds-list = (if (= (length nxt-lvl-vars) 1)
                                             ;; there is only one var, but maybe multiple predicates
                                             (mapcar #'(lambda (predicate) (strip-superfluous-brackets (remove (first nxt-lvl-vars) predicate))) embedded-preds)
                                             ;; there is a corresponding var for each predicate (cityid ?x ?y) (new_york ?x) (ny ?y)
                                             (loop for rem-preds in (copy-object embedded-preds)
                                                   for nxt-lvl-var in nxt-lvl-vars
                                                   do (delete nxt-lvl-var rem-preds)
                                                   collect (strip-superfluous-brackets (list rem-preds))))
        ;; throw out the embedded predicates from remaining predicates
        for remaining-preds = (loop for pred in predicates
                                    unless (member pred embedded-preds)
                                    collect pred)
        when embedded-preds
        ;; substitute the level var with the embedded predicates
        do (if (= (length nxt-lvl-vars) 1)
             ;; there is only one var
             (nsubstitute (if (= 1 (length stripped-embedded-preds-list))
                            ;; there is only one predicate, remove its surrounding brackets
                            (first stripped-embedded-preds-list)
                            ;; there are multiple embedded predicates, pass the full list
                            stripped-embedded-preds-list)
                          (first nxt-lvl-vars) predicate)
             ;; there are multiple vars and corresponding predicates, pair them
             (loop for nxt-lvl-var in nxt-lvl-vars
                   for stripped-embedded-preds in stripped-embedded-preds-list
                   do (nsubstitute stripped-embedded-preds nxt-lvl-var predicate)))
        (setf predicates remaining-preds)
        ;; remove the level var for answer
        finally (return (remove (second (first predicates)) (first predicates)))))


(defun test-pl-to-preds (geo-prolog-string)
  (let* ((geo-prolog (predicates-to-geo-prolog (geo-prolog-to-predicates geo-prolog-string)))
         (result (string= (cl-ppcre:regex-replace-all "[\(\)]" (format nil "~S" (geo-prolog-to-polish-notation geo-prolog-string)) "")
                          (cl-ppcre:regex-replace-all "[\(\)]" (format nil "~S" (list geo-prolog)) ""))))
    (values result geo-prolog)))

(test-pl-to-preds "answer(A,(size(B,A),const(B,cityid('new york',_))))")
(test-pl-to-preds "answer(A,(state(A),const(B,riverid(chattahoochee)),river(B),traverse(B,A)))")
(test-pl-to-preds "answer(A,count(B,(river(B),not((traverse(B,C),state(C),loc(D,C),capital(D),const(D,cityid(albany,_))))),A))")
(test-pl-to-preds "answer(A,count(B,(state(B),not((loc(C,B),river(C)))),A))")
(test-pl-to-preds "answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,stateid(mississippi))))")
(test-pl-to-preds "answer(A,(highest(A,(place(A),loc(A,B),state(B))),lowest(C,(loc(C,B),place(C))),elevation(C,0)))")
(test-pl-to-preds "answer(A,(population(B,A),const(B,cityid(springfield,_))))")
(test-pl-to-preds "answer(A,(state(A),loc(B,A),city(B),const(B,cityid('salt lake city',_))))")
(test-pl-to-preds "answer(C,(state(C),loc(B,C), largest(B,(capital(A,B),city(B),state(A)))))")
(test-pl-to-preds "answer(A,(river(A),traverse(A,B),const(B,cityid(austin,tx))))")
(test-pl-to-preds "answer(A,sum(B,(area(C,B),state(C)),A))")
(test-pl-to-preds "answer(A,(len(B,A),const(B,riverid(colorado)),river(B),loc(B,C),const(C,stateid(texas))))") 
(test-pl-to-preds "answer(A,(state(A),loc(A,B),const(B,countryid(usa)),loc(C,A),const(C,cityid(springfield,_))))")
(test-pl-to-preds "answer(A,count(B,(state(B),next_to(B,C),const(C,stateid(colorado)),next_to(B,D),const(D,stateid('new mexico'))),A))")
(test-pl-to-preds "answer(A,count(B,(river(B),loc(B,C),const(C,stateid(texas)),longer(B,D),const(D,riverid(red))),A))")
(test-pl-to-preds "answer(A,count(B,(city(B),const(B,cityid(austin,_)),loc(B,C),const(C,countryid(usa))),A))")
(test-pl-to-preds "answer(A,sum(B,(population(C,B),state(C),next_to(D,C),const(D,stateid(texas))),A))")
(test-pl-to-preds "answer(A,count(B,(state(B),loc(B,C),const(C,countryid(usa)),shortest(D,river(D)),traverse(D,B)),A))")
(test-pl-to-preds "answer(A,fewest(A,B,(state(A),next_to(A,B),state(B),not((const(A,stateid(alaska)))),not((const(A,stateid(hawaii)))))))")
(test-pl-to-preds "answer(A,count(B,(const(B,riverid(colorado)),river(B)),A))")



;(test-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")

;(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")


