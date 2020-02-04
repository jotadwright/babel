(ql:quickload :clevr-evaluation)

(in-package :clevr-evaluation)
      

(defun exclamation-or-question-p (char) (find char "!?"))

(defun rpn-fn->str (fn)
  "Transform a function in the RPN notation (a list) into a string"
  (downcase
   (remove-if #'exclamation-or-question-p
              (case (length fn)
                (1 (format nil "~a" (first fn)))
                (2 (format nil "~a_~a" (first fn) (second fn)))
                (3 (format nil "~a_~a[~a]" (first fn) (second fn) (third fn)))))))


(defun rpn->str (rpn-list)
  "Transform the RPN notation into a string"
  (let ((list-of-str
         (loop for fn in rpn-list
               for fn-str = (rpn-fn->str fn)
               collect fn-str)))
    (format nil "~{~a~^ ~}" list-of-str)))


(defun succeededp (cipn) (eql (first (statuses cipn)) 'fcg::succeeded))

(defun question-to-rpn (input-line &key grammar (direction '<-))
  "Process a single question into the RPN notation
   and collect it as a csv line."
  (let* ((clean-line (remove #\Return input-line))
         (fields (split clean-line #\,))
         (id (parse-integer (first fields)))
         (utterance (preprocess-sentence (second fields))))
    (multiple-value-bind (meaning cipn)
        (comprehend utterance :cxn-inventory grammar)
      (if (and meaning (succeededp cipn))
          (let ((rpn-str
                 (rpn->str
                  (program->rpn
                   (preprocess-program meaning)))))
            (format nil "~a,~a" id rpn-str))
          (format nil "~a,None" id)))))
  

(defun process-clevr-as-corpus (&key (data-set "val") grammar-configurations
                                     (direction '<->) output-file
                                     number-of-threads lines-per-thread
                                     write-empty-lines-p)
  (let ((corpus-file
         (merge-pathnames
          (make-pathname :directory '(:relative "CLEVR-v1.0" "questions")
                         :name (format nil "CLEVR_~a_corpus" data-set)
                         :type "csv")
          cl-user:*babel-corpora*))
        (corpus-grammar (copy-object *CLEVR*))
        (outputfile (if output-file output-file
                      (babel-pathname :name (format nil "CLEVR_~a_rpn_corpus" data-set) :type "csv"))))
    ;; check if the data-set corpus file exists
    (unless (probe-file corpus-file)
      (error "Could not find the file ~a" corpus-file))
    ;; set the configurations for the grammar
    (loop for (key . value) in grammar-configurations
          when (get-configuration corpus-grammar key)
          do (progn (set-configuration corpus-grammar key value :replace t)
                    (set-configuration (processing-cxn-inventory corpus-grammar) key value :replace t)))
    ;; call process-corpus
    (process-corpus :function #'question-to-rpn
                    :function-kwargs `(:grammar ,corpus-grammar :direction ,direction)
                    :inputfile corpus-file
                    :outputfile outputfile
                    :number-of-threads number-of-threads
                    :number-of-lines-per-thread lines-per-thread
                    :write-empty-lines-p write-empty-lines-p)))

;; testing with mini corpus
#|
(process-clevr-as-corpus :data-set "mini"
                         :direction '<-
                         :grammar-configurations
                         '((:max-nr-of-nodes . 5000))
                         :number-of-threads 4
                         :lines-per-thread 10
                         :write-empty-lines-p nil)
|#

;; Running the training and validation set corpus on the cluster
(process-clevr-as-corpus :data-set "train"
                         :direction '<-
                         :grammar-configurations
                         '((:max-nr-of-nodes . 10000))
                         :number-of-threads 32
                         :lines-per-thread 50
                         :write-empty-lines-p nil)
