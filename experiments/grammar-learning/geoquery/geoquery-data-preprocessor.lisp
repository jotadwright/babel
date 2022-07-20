
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
        append (loop with next-level = (get-next-var-from-var-list var-list)
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
                                              collect next-level))
                     for cand-emb-preds = (loop for el in args
                                                unless (or (numberp el)
                                                     (variable-p el))
                                                collect el)
                     for accum-val = (append (list term) vars)
                     do (when cand-emb-preds
                          (setf next-level (get-next-var next-level)))
                     (cond (;; predicate is a constant e.g. 'new_york
                               (equal (type-of predicate) 'symbol)
                               (setf accum-val (list predicate program-level)))
                              
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
                     finally (when accum-val (setf program-level (first (push (get-next-var-from-var-list var-list) var-list)))))))
        

        

(defun get-next-var-from-var-list (var-list)
  (get-next-var (first (sort var-list #'> :key #'(lambda (x) (position (symbol-name x) gl::+placeholder-vars+ :test #'equal))))))

(defun get-next-var (last-var)
  (intern (nth (+ 1 (position (symbol-name last-var) gl::+placeholder-vars+ :test #'equal)) gl::+placeholder-vars+)))

(defun geo-prolog-to-polish-notation (geo-prolog-string)
  (let* ((geo-prolog-string (cl-ppcre:regex-replace-all "([a-z]+_?[a-z]+)[\(]" geo-prolog-string "\(\\1 "))
         (geo-prolog-string (cl-ppcre:regex-replace-all "([A-Z])" geo-prolog-string "?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all ",(_)" geo-prolog-string ",?\\1"))
         (geo-prolog-string (cl-ppcre:regex-replace-all "'([a-z]+) ([a-z]+)'" geo-prolog-string "\\1_\\2"))
         (geo-prolog-string (replace-all geo-prolog-string "," " "))
         (geo-prolog-string (string-append "("geo-prolog-string ")")))
    (read-from-string geo-prolog-string)))


(geo-prolog-to-polish-notation "answer(A,(state(A),const(B,riverid(chattahoochee)),river(B),traverse(B,A)))")
(geo-prolog-to-predicates "answer(A,(state(A),const(B,riverid(chattahoochee)),river(B),traverse(B,A)))")
#|
(answer ?c ?a ?d)
(state ?d ?a)
(const ?d ?b ?e)
(river ?d ?b)
(traverse ?d ?b ?a)
(riverid ?e ?f)
(chattahoochee ?f)
|#


;; How many rivers do not traverse the state with the capital Albany ?
;; (geo-prolog-to-predicates "answer(A,count(B,(river(B),not((traverse(B,C),state(C),loc(D,C),capital(D),const(D,cityid(albany,_))))),A))")
#|
(answer ?e ?a ?f)
(count ?f ?b ?g ?a)
(river ?g ?b)
(not ?g ?h)
(traverse ?h ?b ?c)
(state ?h ?c)
(loc ?h ?d ?c)
(capital ?h ?d)
(const ?h ?d ?i)
(cityid ?i ?j ?_)
(albany ?j)
|#

(geo-prolog-to-predicates "answer(A,(size(B,A),const(B,cityid('new york',_))))")
#|
(answer ?c ?a ?d)
(size ?d ?b ?a)
(const ?d ?b ?e)
(cityid ?e ?f ?_)
(new_york ?f)

|#


(geo-prolog-to-predicates "answer(A,count(B,(state(B),not((loc(C,B),river(C)))),A))")
 #|
(answer ?d ?a ?e)
(count ?e ?b ?f ?a)
(state ?f ?b)
(not ?f ?g)
(loc ?g ?c ?b)
(river ?g ?c)

    
|#

(geo-prolog-to-predicates "answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,stateid(mississippi))))")
#|
(answer ?d ?a ?e)
(high_point ?e ?b ?a)
(state ?e ?b)
(next_to ?e ?b ?c)
(const ?e ?c ?f)
(stateid ?f ?g)
(mississippi ?g)
"answer(A,(high_point(B,A),state(B),next_to(B,C),const(C,(stateid(mississippi)))))"
|#


;; "What is the highest point in each state whose lowest point is sea level ?"
;; (geo-prolog-to-predicates "answer(A,(highest(A,(place(A),loc(A,B),state(B))),lowest(C,(loc(C,B),place(C))),elevation(C,0)))")


#|
(answer ?d ?a ?e)
(highest ?e ?a ?f)
(lowest ?e ?c ?f) => ?f must be ?g
(elevation ?e ?c 0)
(place ?f ?a)
(loc ?f ?a ?b)
(state ?f ?b)
(loc ?g ?c ?b)
(place ?g ?c)


=> ((FCG::ANSWER FCG::?D FCG::?A FCG::?E) (FCG::HIGHEST FCG::?E FCG::?A FCG::?F) (FCG::LOWEST FCG::?E FCG::?C FCG::?F) (FCG::ELEVATION FCG::?E FCG::?C 0) (FCG::PLACE FCG::?F FCG::?A) (FCG::LOC FCG::?F FCG::?A FCG::?B) (FCG::STATE FCG::?F FCG::?B) (FCG::LOC FCG::?G FCG::?C FCG::?B) (FCG::PLACE FCG::?G FCG::?C))
|#
 
;(geo-prolog-to-predicates "answer(A,(size(B,A),const(B,cityid('new york',_))))")

;(geo-prolog-to-predicates "answer(A,(population(B,A),const(B,cityid(springfield,_))))")

;(parse-geoquery "/Users/u0077062/Projects/babel-corpora/geoquery/geoquery.xml")


