(ql:quickload :cle)

(in-package :cle)

(defun restore-exp (exp-top-dir exp-name log-dir)
  (cl-store:restore (babel-pathname :directory `("experiments"
                                                 "concept-emergence2"
                                                 "logging"
                                                 ,exp-top-dir
                                                 ,exp-name
                                                 ,log-dir
                                                 "stores"
                                                 )
                                    :name "1-history"
                                    :type "store")))

(defmethod equal-exp ((exp1 cle-experiment) (exp2 cle-experiment))
  (loop for ag1 in (agents exp1)
        for ag2 in (agents exp2)
        always (and (equal-lexicon (lexicon ag1)
                                   (lexicon ag2))
                    (equal-window ag1 ag2))))

(defmethod equal-window ((ag1 cle-agent) (ag2 cle-agent))
  (equalp (window (usage-table ag1))
          (window (usage-table ag2))))
                     
(defmethod equal-lexicon ((lex1 list) (lex2 list))
  (loop for cxn1 in lex1
        for cxn2 in lex2
        always (equal-cxn cxn1 cxn2)))

(defmethod equal-cxn ((cxn1 cxn) (cxn2 cxn))
  (and (equalp (form cxn1)
               (form cxn2))
       (equalp (score cxn1)
               (score cxn2))))

(defun get-directory (path)
  (last-elt (pathname-directory path)))

(defun get-seed (path)
  (subseq (fourth
           (split-sequence:split-sequence #\- (get-directory path)))
          4))

(defun index-order-for-sorting (first-list second-list)
  (let ((position-map (make-hash-table :test 'equal)))
    ;; Create a hash table to map each string in first-list to its position
    (loop for item in first-list
          for index from 0
          do (setf (gethash item position-map) index))
    ;; Generate a list of indexes from second-list that would sort it according to first-list
    (loop for item in second-list
          collect (gethash item position-map))))

(defun sort-list-by-indexes (list indexes)
  (let ((sorted-list (make-array (length list))))
    ;; Fill sorted-list with elements from list based on indexes
    (loop for index in indexes
          for i from 0
          do (setf (aref sorted-list i) (nth index list)))
    (coerce sorted-list 'list)))

(defun compare-experiments (exp-top-dir1 exp-top-dir2)
  (loop with exp-names1 = (uiop:subdirectories (babel-pathname
                                                :directory `("experiments"
                                                             "concept-emergence2"
                                                             "logging"
                                                             ,exp-top-dir1
                                                             )))
        with exp-names2-unsorted = (uiop:subdirectories (babel-pathname
                                                         :directory `("experiments"
                                                                      "concept-emergence2"
                                                                      "logging"
                                                                      ,exp-top-dir2
                                                                      )))
        ;for exp-name1 in exp-names1
        ;for exp-name2-unsorted in exp-names2
        for exp-name-dirs1 = (mapcar #'get-directory exp-names1)
        for exp-name-dirs2 = (mapcar #'get-directory exp-names2-unsorted)
        for exp-names2 = (sort-list-by-indexes
                          exp-names2-unsorted
                          (index-order-for-sorting exp-name-dirs1 exp-name-dirs2))
        for exp-name1 in exp-names1
        for exp-name2 in exp-names2
        for log-dirs1 = (uiop:subdirectories exp-name1)
        for log-dirs2-unsorted = (uiop:subdirectories exp-name2)
        for seeds1 = (mapcar #'get-seed log-dirs1)
        for seeds2 = (mapcar #'get-seed log-dirs2-unsorted)
        for log-dirs2 = (sort-list-by-indexes log-dirs2-unsorted (index-order-for-sorting seeds1 seeds2))
        for equality = (loop for log-dir1 in log-dirs1
                             for log-dir2 in log-dirs2
                             for exp1 = (restore-exp exp-top-dir1
                                                     (get-directory exp-name1)
                                                     (get-directory log-dir1))
                             for exp2 = (restore-exp exp-top-dir2
                                                     (get-directory exp-name2)
                                                     (get-directory log-dir2))
                             always (equal-exp exp1 exp2))
        always equality))

#|

;; compare individual runs
(progn
  (setf *exp1* (restore-exp "run1" "winery" "2024-08-1_0h6m30s-seed3-99999"))
  (setf *exp2* (restore-exp "run2" "winery" "2024-08-1_0h6m30s-seed3-01010"))
  (equal-exp *exp1* *exp2*))

;; compare entire top-level-dirs
(compare-experiments "run1" "run2")
|#