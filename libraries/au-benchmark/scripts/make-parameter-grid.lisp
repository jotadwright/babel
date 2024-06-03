(ql:quickload :au-benchmark)
(in-package :au-benchmark)

;;;; Make csv file for grid search

(defparameter *file-options*
  '("class-1.lisp" "class-2.lisp"
    "class-3.lisp" "class-4.lisp"
    "class-5.lisp" "class-6.lisp"))

(defparameter *k-options*
  '(0 2 4 nil))

(defparameter *W-options*
  '(1 2 4))

(defparameter *V-options*
  '(1 2 4))

(defparameter *omega-scope-options*
  '("global"))

(defparameter *parameter-file*
  (make-pathname
   :directory '(:absolute "Users" "jensnevens" "quicklisp"
                "local-projects" "k-swap-anti-unification"
                "code" "scripts")
   :name "parameters_kswap_lcg"
   :type "csv"))

(defun listXlist (l1 l2 &key (combiner #'list))
  (declare (list l1) (list l2) (function combiner))
  (loop for el1 in l1 nconc
        (loop for el2 in l2 collect
               (funcall combiner el1 el2))))

(defun combinations (&rest lists)
  (reduce #'(lambda (ac new) 
              (listXlist new ac :combiner #'cons))
          (push '(()) lists)))

(defun make-parameter-grid ()
  (with-open-file (stream *parameter-file* :direction :output
                          :if-exists :supersede)
    (write-line "file, k, W, V, scope" stream)
    (loop for combination in (combinations *file-options*
                                           *k-options*
                                           *W-options*
                                           *V-options*
                                           *omega-scope-options*)
          when (= (second combination) (third combination))
          do (write-line
              (format nil "~{~a~^,~}" (reverse combination))
              stream))))

; (make-parameter-grid)



;;;; Extract the jobs that failed...

(defparameter *failed-jobs-file*
  (parse-namestring "/Users/jensnevens/quicklisp/local-projects/k-swap-anti-unification/code/results/failed-jobs.txt"))

(defparameter *failed-jobs-ids*
  (with-open-file (stream *failed-jobs-file* :direction :input)
    (sort
     (loop for line = (read-line stream nil nil)
           while line
           for data = (split-sequence:split-sequence #\space line)
           for filename = (first (last data))
           for array-id = (parse-integer (third (split-sequence:split-sequence #\- filename)))
           collect array-id)
     #'<)))

;; (format nil "[~{~a~^,~}]" '(1 9 18 19 21 23 25 47 49 58 60 64 71 77 87 91 98 99 102 104 109 110 111 112 114 117 121 122 139 140 146 153 156))
;; [1,9,18,19,21,23,25,47,49,58,60,64,71,77,87,91,98,99,102,104,109,110,111,112,114,117,121,122,139,140,146,153,156]


