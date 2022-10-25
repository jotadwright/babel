;; data-processing.lisp

(ql:quickload :clevr-learning)
(in-package :clevr-learning)

(defun json-to-lsp (file)
  (let* ((alist
          (cl-json:decode-json-from-source file))
         (data
          (list (rest (assoc :question alist))
                (read-from-string (rest (assoc :meaning alist)))
                (loop for entry in (rest (assoc :answers alist))
                      collect (cons (rest (assoc :scene entry))
                                    (let ((a (rest (assoc :answer entry))))
                                      (cond ((string= a "yes") t)
                                            ((string= a "no") nil)
                                            ((null (parse-integer a :junk-allowed t))
                                             (internal-symb (upcase a)))
                                            (t (parse-integer a))))))))
         (outfile
          (make-pathname :directory (pathname-directory file)
                         :name (pathname-name file) :type "lisp")))
    (with-open-file (stream outfile :direction :output
                            :if-exists :supersede)
      (write data :stream stream))))

(defun convert-all-files-in-dir (dir)
  (let ((all-files
         (directory
          (make-pathname :directory (pathname-directory dir)
                         :name :wild :type "txt"))))
    (with-progress-bar (bar (length all-files) ("Converting files..."))
      (loop for file in all-files
            do (json-to-lsp file)
            do (update bar)))))

;; First version of CLEVR learning data was stored in JSON format
;; Convert these to .lisp files as these take up less memory and
;; they can be read in memory faster
#|
(convert-all-files-in-dir
 (parse-namestring "/Users/jensnevens/Babel-Corpora/clevr-learning-data/val/stage-3/"))
|#





(defun set-default-clevr-grammar-configurations ()
  (set-configurations *fcg-constructions*
                      '((:cxn-supplier-mode . :ordered-by-label-hashed)
                        (:priority-mode . :nr-of-applied-cxns)
                        (:parse-order hashed nom cxn)
                        (:production-order hashed-lex nom cxn hashed-morph)
                        (:max-nr-of-nodes . 10000))))

(defun answer->str (answer-value)
  (case #+lispworks (type-of answer-value)
        #+ccl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
        #+sbcl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
    (number (mkstr answer-value))
    (fixnum (mkstr answer-value))
    (integer (mkstr answer-value))
    (bit (mkstr answer-value))
    (shape-category (mkstr (shape answer-value)))
    (size-category (mkstr (size answer-value)))
    (color-category (mkstr (color answer-value)))
    (material-category (mkstr (material answer-value)))
    (boolean-category (mkstr (id answer-value)))))

(defun answer-question-in-scene (irl-program scene)
  (set-data *clevr-ontology* 'clevr-context scene)
  (let ((solutions
         (evaluate-irl-program irl-program *clevr-ontology*)))
    (if (and solutions (= (length solutions) 1))
      (let* ((answer
              (answer->str
               (get-target-value irl-program (first solutions)))))
        (values (name scene) (mkstr (downcase answer))))
      (values nil nil))))
  
(defun answer-all-questions-in-all-scenes (inputfile outputdir dataset)
  ;; open streams
  (let ((in-stream
         (open inputfile :direction :input))
        (world
         (make-instance 'clevr-world :data-sets (list dataset))))
    ;; skip the header line
    (read-line in-stream nil nil)
    ;; loop over each line
    (loop for line = (read-line in-stream nil nil)
          for index from 0
          while line
          for fields = (split line #\,)
          for question = (second fields)
          ;; comprehend it to get the meaning
          for (irl-program cipn) = (multiple-value-list (comprehend question))
          when (eql (first (statuses cipn)) 'fcg::succeeded)
          ;; execute the meaning in every scene
          do (with-progress-bar (bar (length (scenes world))
                                     ("Processing question \"~a\"" question))
               (let ((counter 0)
                     (answers nil))
                 (do-for-scenes
                  world
                  (lambda (scene)
                    (multiple-value-bind (scene-name answer)
                        (answer-question-in-scene irl-program scene)
                      (update bar)
                      ;; when an answer is computed, store it
                      (when answer
                        (incf counter)
                        (push (cons scene-name answer) answers)
                        t))))
                 (let* ((file-data
                         (list question irl-program answers))
                        (file-name
                         (format nil "question_~6,'0d" index))
                        (file-path
                         (merge-pathnames
                          (make-pathname :name file-name :type "lisp")
                          outputdir)))
                   ;; TO DO; add level-1, level-2 and level-3 splits!
                   (ensure-directories-exist file-path)
                   (with-open-file (out-stream file-path :direction :output
                                               :if-exists :supersede
                                               :if-does-not-exist :create)
                     (write file-data :stream out-stream)
                     (force-output out-stream))
                   (sleep 1)))))))

(defun enhance-data (inputfile outputdir dataset)
  (set-default-clevr-grammar-configurations)
  (answer-all-questions-in-all-scenes inputfile outputdir dataset))

#|
(enhance-data
 (merge-pathnames
  (make-pathname :directory '(:relative "seq2seq" "data")
                 :name "CLEVR_train_corpus_comprehension" :type "csv")
  cl-user:*babel-corpora*)
 (merge-pathnames
  (make-pathname :directory '(:relative "clevr-learning-data" "train"))
  cl-user:*babel-corpora*)
 "train")
|#
 










