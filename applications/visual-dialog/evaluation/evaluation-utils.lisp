(in-package :visual-dialog)

(defmethod guess-answer (irl-program (dataset (eql :clevr)))
  "Guess an answer from a distributed list of possible answers based on the target primitive."
  (let* ((target-var (get-target-var irl-program))
         (target-predicate (find target-var irl-program :key #'second :test #'eql))
         (target-prim (first target-predicate)))
    (cond ((eq target-prim 'clevr-dialog-grammar:exist)
           (guess-exist-distr))
          ((eq target-prim 'count-objects)
           (guess-count-distr dataset))
          ((eq target-prim 'query)
           (guess-query-distr (last-elt (find (last-elt target-predicate) irl-program :key #'third :test #'eql)) dataset)))))

(defmethod guess-answer (irl-program (dataset (eql :mnist)))
  "Guess an answer from a distributed list of possible answers based on the target primitive."
  (let* ((target-var (get-target-var irl-program))
         (target-predicate (find target-var irl-program :key #'second :test #'eql))
         (target-prim (first target-predicate)))
    (cond ((eq target-prim 'count-objects)
           (guess-count-distr dataset))
          ((eq target-prim 'query)
           (guess-query-distr (last-elt (find (last-elt target-predicate) irl-program :key #'third :test #'eql)) dataset)))))


(defun guess-exist-distr ()
  (let ((exist-list (concatenate 'list
             (loop for i from 1 to 66 collect (intern "YES"))
             (loop for j from 1 to 33 collect (intern "NO")))))
    (nth (random (list-length exist-list)) exist-list)))


(defmethod guess-count-distr ((dataset (eql :mnist)))
  (let ((prob-list (loop for num in (range 10)
                         for prob in '(5 44 22 11 6 4 2 2 1 1 1)
                         append (loop for el from 1 to prob
                                      collect (internal-symb (upcase (format nil "~r" num)))))))
    (nth (random (list-length prob-list)) prob-list)))

(defmethod guess-count-distr ((dataset (eql :clevr)))
  (let ((prob-list (loop for num in (range 10)
                         for prob in '(16 32 14 11 8 6 4 3 2 1 1)
                         append (loop for el from 1 to prob
                                      collect (internal-symb (upcase (format nil "~r" num)))))))
    (nth (random (list-length prob-list)) prob-list)))

  
(defmethod guess-query-distr (attr (dataset (eql :clevr)))
  (let ((shape-list (concatenate 'list
                                (loop for i from 1 to 33 collect (intern (upcase "cube")))
                                (loop for j from 1 to 33 collect (intern (upcase "sphere")))
                                (loop for k from 1 to 33 collect (intern (upcase "cylinder")))
                                (loop for k from 1 to 1 collect (intern (upcase "none")))))
        (color-list (concatenate 'list
                                (loop for i from 1 to 12 collect (intern (upcase "yellow")))
                                (loop for j from 1 to 12 collect (intern (upcase "gray")))
                                (loop for k from 1 to 12 collect (intern (upcase "brown")))
                                (loop for i from 1 to 12 collect (intern (upcase "green")))
                                (loop for j from 1 to 12 collect (intern (upcase "purple")))
                                (loop for k from 1 to 13 collect (intern (upcase "blue")))
                                (loop for i from 1 to 12 collect (intern (upcase "cyan")))
                                (loop for j from 1 to 13 collect (intern (upcase "red")))
                                (loop for k from 1 to 1 collect (intern (upcase "none")))))
        (size-list (concatenate 'list
                                (loop for i from 1 to 47 collect (intern (upcase "large")))
                                (loop for j from 1 to 52 collect (intern (upcase "small")))
                                (loop for k from 1 to 1 collect (intern (upcase "none")))))
        (material-list (concatenate 'list
                                (loop for i from 1 to 49 collect (intern (upcase "metal")))
                                (loop for j from 1 to 49 collect (intern (upcase "rubber")))
                                (loop for k from 1 to 1 collect (intern (upcase "none"))))))
  (cond ((eq attr 'shape)
         (nth (random (list-length shape-list)) shape-list))
        ((eq attr 'color)
         (nth (random (list-length color-list)) color-list))
        ((eq attr 'size)
         (nth (random (list-length size-list)) size-list))
        ((eq attr 'material)
         (nth (random (list-length material-list)) material-list)))))

(defmethod guess-query-distr (attr (dataset (eql :mnist)))
  (let ((style-list (loop for a in (list "flat" "stroke")
                         collect (intern (upcase a))))
        (color-list (loop for a in (list "violet" "blue" "red" "green" "brown")
                         collect (intern (upcase a))))
        (bgcolor-list (loop for a in (list "salmon" "cyan" "yellow" "silver" "white")
                         collect (intern (upcase a))))
        (number-list (loop for a in (loop for num from 0 to 9 collect (mkstr num))
                         collect (intern (upcase a)))))
  (cond ((eq attr 'style)
         (nth (random (list-length style-list)) style-list))
        ((eq attr 'color)
         (nth (random (list-length color-list)) color-list))
        ((eq attr 'bgcolor)
         (nth (random (list-length bgcolor-list)) bgcolor-list))
        ((eq attr 'number)
         (nth (random (list-length number-list)) number-list)))))

(defun calculate-accuracy-from-dir (dir)
  (let* ((files (directory dir))
         (results
          (loop for file in files
                for file-content = (open file)
                append (result-file->result-list file-content))))
    (average results)))

(defun result-file->result-list (stream &key number-of-lines)
  "collect all the lines that are results"
    (loop for line = (read-line stream nil nil)
          while line
          when (and (not (string= (first-word line) "evaluation"))
                    (not (string= (first-word line) "dialog-level-accuracy"))
                    (not (string= (first-word line) "question-level-accuracy")))
            append (read-from-string (last-elt (split-string line ":")))))

(defun collect-failed-dialogs (dir)
   (let* ((files (directory dir))
          (failed-dialogs
           (loop for file in files
                 for file-content = (open file)
                 for question-result = (last-elt (split-string (last-elt (stream->list file-content)) " "))
                 when (not (equal question-result "1.0"))
                   collect file
                 do (close file-content))))
     failed-dialogs))

(defun check-failed-dialogs (failed-dialogs multiple-middles-file)
  (with-open-file (str multiple-middles-file)
    (let ((middles-list (read-from-string (read-line  str))))
      (loop for dialog in failed-dialogs
            do (with-open-file (dialog-file dialog)
                 (let ((lines (stream->list dialog-file)))
                   (loop for line in lines
                           when (and (not (string= (first-word line) "dialog-level-accuracy"))
                                     (not (string= (first-word line) "question-level-accuracy")))
                           do (let* ((split-line (split-string line ":"))
                                     (scene (parse-integer (first (split-string (first split-line) ","))))
                                     (results (read-from-string (last-elt split-line))))
                                (if (and (not (equal (average results) 1.0))
                                         (not (find scene middles-list)))
                                  (format t "~a~%" line))))))))))

                

(defun collect-problematic-middle-scenes ()
  (let* ((ontology (build-ontology))
         (world (make-instance 'world :entries '((:dataset . :clevr)
                                                 (:datasplit . :train)
                                                 (:mode . :symbolic))))
         (multiple-middles
          (loop with i = 0
                for scene in (scenes world)
                  
                for s = (get-scene-by-index world i)
                for context = (make-context world)
                for number-of-middles = (length (middle (scene-configuration (object-set (first (set-items context))))))
                if (< number-of-middles 2)
                  do (progn (print i)
                       (incf i))
                else
                  collect i
                  and do (progn (print i)
                           (incf i))))
         (outfile (babel-pathname :directory '("applications" "visual-dialog" "evaluation")
                               :name "scenes-with-multiple-middles"
                               :type "lisp")))
    (with-open-file (str outfile
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format str "~a" multiple-middles) (force-output str))
    multiple-middles))


(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))
                 
