
(ql:quickload :predicate-networks)
(in-package :pn)

(defun atom-string->set-of-predicates (atom-string)
  (let ((fn (compose #'(lambda (str) (string-replace str "_" "-"))
                     #'(lambda (str) (cl-ppcre:regex-replace-all "([a-z_]*)[\(]" str "\(\\1 "))
                     #'(lambda (str) (cl-ppcre:regex-replace-all "," str " "))
                     #'(lambda (str) (format nil "(~a)" str))
                     #'(lambda (str) (cl-ppcre:regex-replace-all "([A-Z])" str "\?\\1")))))
    (read-from-string (funcall fn atom-string))))

(defun set-of-predicates->atom-string (set-of-predicates)
  (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
         (unique-variables (remove-duplicates all-variables))
         (mapping
          (loop for var in unique-variables
                for i from 1
                collect (cons var (format nil "'$VAR'(~a)" i))))
         (predicates-lists
          (loop for predicate in set-of-predicates
                collect (cons (downcase (mkdotstr (car predicate)))
                              (loop for arg in (rest predicate)
                                    if (variable-p arg)
                                    collect (rest (assoc arg mapping))
                                    else collect (downcase (mkdotstr arg))))))
         (predicates-strings
          (loop for predicate in predicates-lists
                collect (format nil "~a(~{~a~^,~})"
                                (first predicate)
                                (rest predicate)))))
    (list-of-strings->string predicates-strings :separator ",")))

;; #############################################

#|
(atom-string->set-of-predicates
 "f(A),f(B),f(C),g(A,C),g(B,A),g(C,D),g(D,A),g(D,D),h(A,D,D),h(B,C,A),h(B,C,B),h(C,A,A),h(D,C,C)")
(atom-string->set-of-predicates
 "f(F),g(E,G),g(H,G),h(G,H,E)")
(atom-string->set-of-predicates "f(C),g(B,A),g(D,A)")
  
(set-of-predicates->atom-string
 '((get-context ?context)
   (filter ?set-1 ?context ?cube)
   (bind shape-category ?cube cube)
   (exist ?target ?set-2)))

get_context(Context),filter(Set_1,Context,Cube),bind(shape_category,Cube,cube),exist(Target,Set_2)

(set-of-predicates->atom-string
 '((get-context ?c)
   (filter ?s1 ?c ?b1)
   (bind shape-category ?b1 sphere)
   (exist ?t ?s2)))

get_context(C),filter(S1,C,B1),bind(shape_category,B1,sphere),exist(T,S2)

(atom-string->set-of-predicates
 (set-of-predicates->atom-string
  '((get-context ?context)
   (filter ?set-1 ?context ?cube)
   (bind shape-category ?cube cube)
   (filter ?set-2 ?set-1 ?color-1)
   (bind color-category ?color-1 red)
   (exist ?target ?set-2))))
|# 
