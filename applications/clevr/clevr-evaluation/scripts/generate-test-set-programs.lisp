
(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and Output Files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *clevr-test-corpus-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "CLEVR-v1.0" "questions")
                  :name "CLEVR_test_corpus" :type "csv")
   cl-user:*babel-corpora*))

(defparameter *output-path*
  (merge-pathnames
   (make-pathname :directory '(:relative "CLEVR-v1.0" "questions")
                  :name "CLEVR_test_programs" :type "csv")
   cl-user:*babel-corpora*))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun succeededp (cipn)
  "Check if the goal test succeeded"
  (and cipn (eql (first (statuses cipn)) 'fcg::succeeded)))

(defun clevr-comprehend (utterance)
  "Comprehend the utterance and return the
   meaning and the cipn"
  (multiple-value-bind (irl-program cipn)
      (clevr-grammar::understand utterance)
    (if (succeededp cipn)
      (values irl-program cipn)
      (error "Failed to comprehend \"~a\"" utterance))))

;;;;;;;;;;;;;;;;;;;
;; Main Function ;;
;;;;;;;;;;;;;;;;;;;

(defun process-line (line-from-corpus)
  (let* ((parts (split line-from-corpus #\,))
         (question (second parts))
         (irl-program (clevr-comprehend question))
         (json-program
          (tree->json
           (program->tree
            (preprocess-program irl-program)))))
    (encode-json-to-string json-program)))

(defun main ()
  (process-corpus-with-threads
   :function #'process-line
   :inputfile *clevr-test-corpus-path*
   :outputfile *output-path*
   :number-of-threads 12
   :number-of-lines-per-thread 100))

#-lispworks (main)
