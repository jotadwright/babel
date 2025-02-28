(ql:quickload :cle)
(in-package :cle)

(loop for i from 1 to 37
        collect i)
(list 1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20 23 24 25 26 27 28 29 30 31 32 34 35 36 37)


(defun export-lexicon (experiment base-path)
  (loop with agent = (first (agents experiment))
        for inventory in (list :fast) ;:trash)
        do (loop with lexicon =  (hash-values (get-inventory (lexicon agent) inventory))
                 with sorted-lexicon = (sort lexicon #'(lambda (x y) (> (score x) (score y))))
                 for cxn in sorted-lexicon
                 do (s-dot->image
                     (cxn->s-dot cxn :weight-threshold 0.1)
                     :path (merge-pathnames
                            (make-pathname :name (format nil "~,2f-~a-cxn" (score cxn) (form cxn))
                                           :type "pdf")
                            base-path)
                     :format "pdf"
                     :open nil))))


(defun smcl2 ()
  (loop for i in (list 13 23 37) ;; 
        for exp-name = (format nil "smcl2-~a" i)
        for fdir = (babel-pathname :directory `("experiments"
                                                "concept-emergence2"
                                                "storage"
                                                "cle3-smcl2-nopowerset"
                                                "stores"
                                                ,exp-name))
        for *experiment* = (load-experiment fdir "seed-1")
        for base-path = (babel-pathname :directory `("experiments"
                                                     "concept-emergence2"
                                                     "storage"
                                                     "cle3-smcl2-nopowerset"
                                                     "lexicons"
                                                     ,exp-name))
        do (ensure-directories-exist base-path)
        do (export-lexicon *experiment* base-path)))

(smcl2)

(defun clevr ()
  (loop for i in (list 'a)
        for top-exp-dir = "conll-cogent"
        for exp-name = (format nil "clevr" i)
        for fdir = (babel-pathname :directory `("experiments"
                                                "concept-emergence2"
                                                "storage"
                                                ,top-exp-dir
                                                "stores"
                                                ,exp-name))
        for *experiment* = (load-experiment fdir "seed-1")
        for base-path = (babel-pathname :directory `("experiments"
                                                     "concept-emergence2"
                                                     "storage"
                                                     ,top-exp-dir
                                                     "lexicons"
                                                     ,exp-name))
        do (ensure-directories-exist base-path)
        do (export-lexicon *experiment* base-path)))

(clevr)