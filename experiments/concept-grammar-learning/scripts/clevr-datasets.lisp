(in-package :clg)


(defun order-files-in-folders (dir &key
                                   (objects-and-things t)
                                   (length-subdirs t))
  (let ((query-questions nil) (exist-questions nil) (count-questions nil))
    (loop with lexicals = (append '("blue" "brown" "cyan" "gray" "green" "purple" "red" "yellow")
                                  '("metal" "rubber")
                                  '("small" "large")
                                  '("cube" "cylinder" "sphere"))
          with synonyms = (remove nil (loop for form in lexicals append (get-synonyms form)))
          with file-dir = (directory (merge-pathnames dir "*.lisp"))
          
          for file in file-dir
          for file-data = (with-open-file (stream file :direction :input) (read stream))
          for count-question-p = (find 'count! (second file-data) :key #'first)
          for query-question-p = (find 'query (second file-data) :key #'first)
          for exist-question-p = (find 'exist (second file-data) :key #'first)
          for question = (first file-data)
          for synonyms-p = (loop for synonym in synonyms
                                 when (search synonym question)
                                   collect synonym)
          for objects-or-things-p = (loop for synonym in '("thing" "things" "object" "objects")
                                          when (search synonym question)
                                            collect synonym)
          if (and (not synonyms-p)  count-question-p
                  (not (find question count-questions :test #'string=)))
            do (if objects-or-things-p
                 (when objects-and-things (write-to-file file-data dir "count-with-objects/" file :length-subdirs length-subdirs))
                 (progn
                   (write-to-file file-data dir "count/" file :length-subdirs length-subdirs)
                   (when objects-and-things(write-to-file file-data dir "count-with-objects/" file :length-subdirs length-subdirs))))
            and do (push question count-questions)
          if (and (not synonyms-p)  query-question-p
                  (not (find question query-questions :test #'string=)))
            do (if objects-or-things-p
                 (when (write-to-file file-data dir "query-with-objects/" file :length-subdirs length-subdirs))
                 (progn
                   (write-to-file file-data dir "query/" file :length-subdirs length-subdirs)
                   (when (write-to-file file-data dir "query-with-objects/" file :length-subdirs length-subdirs))))
            and do (push question query-questions)
          if (and (not synonyms-p)  exist-question-p
                  (not (find question exist-questions :test #'string=)))
            do (if objects-or-things-p
                 (when (write-to-file file-data dir "exist/" file :length-subdirs length-subdirs))
                 (progn
                   (write-to-file file-data dir "exist-with-objects/" file :length-subdirs length-subdirs)
                   (when (write-to-file file-data dir "exist/" file :length-subdirs length-subdirs))))
            and do (push question exist-questions))))


(defun write-to-file (content dir subdir file &key (length-subdirs t))
  (when (not (string= (file-namestring file) ".DS_store"))
    (let* ((subsubdir (format nil "~a~a/" subdir (first (split-string
                                                         (last-elt (split-string (file-namestring file) "_"))
                                                         "."))))
           (path (merge-pathnames (file-namestring file) (merge-pathnames (if length-subdirs subsubdir subdir) dir))))
      (with-open-file (stream path :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create)
        (write content :stream stream)))))





(order-files-in-folders "/Users/lara/Projects/babel/../corpora/CLEVR-intention-reading-data/val/stage-1/"
                            :objects-and-things t
                            :length-subdirs t)

(order-files-in-folders "/Users/lara/Projects/babel/../corpora/CLEVR-intention-reading-data/val/stage-1/"
                            :objects-and-things nil
                            :length-subdirs nil)

(progn
  (order-files-in-folders "/Users/lara/Projects/babel/../corpora/CLEVR-intention-reading-data/val/stage-1/"
                          :objects-and-things nil
                          :length-subdirs t)
  (order-files-in-folders "/Users/lara/Projects/babel/../corpora/CLEVR-intention-reading-data/val/stage-1/"
                          :objects-and-things nil
                          :length-subdirs nil))


(first (with-open-file (stream (first (directory (merge-pathnames "/Users/lara/Projects/babel/../corpora/CLEVR-intention-reading-data/val/stage-1-all/count/" "*.lisp"))) :direction :input) (read stream)))