
;;;;; Creating distance matrices between sentences (that share the same key phrase)
;;;;; Katrien Beuls and Paul Van Eecke
;;;;;
;;;;; 
;;;;;
;;;;; 

;;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the local Guardian data in the corpora-svn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The original Guardian corpus (14 GB)
(defparameter *corpus-json* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "guardian-data"
                                            :type "json"))

(unless (probe-file *corpus-json*) (warn "The guardian-data.json file was not found. Please check the corpora SVN."))

;; The Guardian corpus without the html field (1.3 GB)
(defparameter *corpus-json-no-html* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "guardian-data-no-html"
                                            :type "json"))

(unless (probe-file *corpus-json-no-html*) (warn "The Guardian guardian-data-no-html.json file was not found. Please check the corpora SVN."))

;; A corpus containing the article texts of the Guardian Corpus (75 MB)
(defparameter *corpus-json-article-texts* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "guardian-article-texts"
                                            :type "json"))

(unless (probe-file *corpus-json-article-texts*) (warn "The Guardian guardian-article-texts.json file was not found. Please check the corpora SVN."))

(defparameter *corpus-json-article-texts-withcomments* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                                       :name "guardian-article-texts-with-comments"
                                                                       :type "json"))
(unless (probe-file *corpus-json-article-texts-withcomments*) (warn "The Guardian guardian-article-texts-with-comments.json file was not found. Please check the corpora SVN."))

(defparameter *corpus-json-article-sentences-per-article* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                                          :name "guardian-article-sentences"
                                                                          :type "json"))

(defparameter *corpus-json-article-sentences-per-article-including-comments* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                                                             :name "guardian-article-sentences-including-comments"
                                                                                             :type "json"))

(defparameter *corpus-json-article-sentences* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                                                          :name "guardian-article-sentences-one-sentence-per-line"
                                                                          :type "json"))

(defparameter *corpus-json-article-sentences-all*
  (babel-pathname :directory '(:up "Corpora" "Guardian")
                  :name "guardian-article-sentences-one-sentence-per-line-including-comments"
                  :type "json"))

(defparameter *turnbull-test-in*
  (babel-pathname :directory '(:up "Corpora" "Guardian")
                  :name "turnbull-test-in"
                  :type "json"))

(defparameter *turnbull-sentences*
  (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                  :name "turnbull-sentences"
                  :type "json"))

(defparameter *turnbull-vectors*
  (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                  :name "turnbull-vectors"
                  :type "json"))


(defparameter *corpus-json-article-sentences-lemmatized* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "guardian-article-sentences-lemmatized"
                                            :type "json"))

;(unless (probe-file *corpus-json-article-sentces-lemmatized*) (warn "The Guardian guardian-article-sentences-lemmatized.json file was not found. Please check the corpora SVN."))

(defparameter *corpus-json-article-sentences-filtered* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "guardian-article-sentences-filtered"
                                            :type "json"))

(defparameter *frequency-count-file* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "frequency-counts"
                                            :type "json"))

;(unless (probe-file *corpus-json-article-sentences-filtered*) (warn "The Guardian guardian-article-sentences-filtered file was not found. Please check the corpora SVN."))

(defparameter *stopwords* (let ((stopword-hash-table (make-hash-table :test #'equalp)))
                            (loop for stopword in '("a" "able" "about" "across" "after" "all" "almost" "also" "am" "among" "an" "and" "any" "are" "as" "at" "be" "because"
                                                    "been" "but" "by" "can" "cannot" "could" "dear" "did" "do" "does" "either" "else" "ever" "every" "for" "from" "get"
                                                    "got" "had" "has" "have" "he" "her" "hers" "him" "his" "how" "however" "i" "if" "in" "into" "is" "it" "its" "just"
                                                    "least" "let" "like" "likely" "may" "me" "might" "most" "must" "my" "neither" "no" "nor" "not" "of" "off" "often" "on"
                                                    "only" "or" "other" "our" "own" "rather" "said" "say" "says" "she" "should" "since" "so" "some" "than" "that" "the" "their"
                                                    "them" "then" "there" "these" "they" "this" "tis" "to" "too" "twas" "us" "wants" "was" "we" "were" "what" "when" "where"
                                                    "which" "while" "who" "whom" "why" "will" "with" "would" "yet" "you" "you" "-PRON-" "[" "]")
                                  do (setf (gethash stopword stopword-hash-table) t))
                            stopword-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving or doing things with individual JSON objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun lemmatize-and-clean-sentence-json-object (json-object)
  (let* ((sentence (cdr (assoc :sentence  (decode-json-from-string json-object))))
         (article-id (assoc :article-id  (decode-json-from-string json-object)))
         (sentence-index (assoc :sentence-index  (decode-json-from-string json-object)))
         (sentence-id (assoc :sentence-id  (decode-json-from-string json-object))))
    (encode-json-alist-to-string `(,sentence-id
                                   ("sentence"  . ,(lemmatize-and-clean-sentence sentence))
                                   ,article-id
                                   ,sentence-index))))

(defun lemmatize-and-clean-sentence (string)
  (let ((string string))
    ;; lemmatizing
    (setf string (format nil "~{~a ~}" (get-penelope-lemmas string)))
    ;; removing punctuation
    (setf string (remove-punctuation-not-numbers string))
    (setf string (string-trim '(#\Space) (format nil "~{~a ~}" (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))))
    ;; string consists only of punctuation
    (when (reduce #'always (map 'list #'utils::punctuation-p string))
      (setf string ""))
    string))



(defun extract-sentences-from-json-object-one-per-line (json-object)
  "Takes a json object (one article) and returns a string, where every line is in a new json object for a sentence."
  (let* ((decoded-object (decode-json-from-string json-object))
         (article-text (cdr (assoc :text decoded-object)))
         (article-comments (cdr (assoc :comments decoded-object)))
         (article-id (cdr (assoc :--id decoded-object)))
         (sentences (sentence-tokenize-and-clean-text article-text)) ;;returns list
         sentence-json-lines
         comment-sentence-json-lines)

    (setf sentence-json-lines
          (format nil "~{~a~%~}" (loop for sentence in sentences
                                          for sentence-index from 1
                                          collect (encode-json-alist-to-string
                                                   `(("sentence" . ,(list-of-strings->string sentence))
                                                     ("type" . "article")
                                                     ("article-id" . ,article-id)
                                                     ,(assoc :author (decode-json-from-string json-object))
                                                     ("sentence-index" . ,sentence-index))))))
    (if article-comments
      (progn
        (setf comment-sentence-json-lines
          (format nil "~{~a~%~}"
                  (loop for json-comment in article-comments
                        for comment-text = (cdr (assoc :text json-comment))
                        for comment-sentences = (sentence-tokenize-and-clean-text comment-text)
                        append (loop for sentence in comment-sentences
                                     for sentence-index from 1
                                     collect (encode-json-alist-to-string
                                              `(("sentence" . ,(list-of-strings->string sentence))
                                                ("type" . "comment")
                                                ("article-id" . ,article-id)
                                                ,(assoc :author json-comment)
                                                ("sentence-index" . ,sentence-index)
                                                (:comment-time . ,(cdr (assoc :$date (cdr (assoc :time--stamp json-comment)))))))))))
        (format nil "~a~%~a"
                sentence-json-lines
                comment-sentence-json-lines))
      sentence-json-lines)))


#|(extract-sentences-from-json-object-one-per-line
 (encode-json-to-string (get-json-object-from-file  *corpus-json-article-texts-withcomments* 5))) |#


(defun sentence-tokenize-and-clean-text (single-text-as-string &key
                                                               (remove-newlines t))
  "Takes a single text as input in the form of a string and returns a list containing the text's sentences tokenized."
    ;; Removing newlines
    (when remove-newlines
      (setf single-text-as-string (remove-newlines single-text-as-string)))
    ;; Full tokenization in one go
   (first (get-penelope-text-tokens (list single-text-as-string)))) ;;single text so first elt

;; (sentence-tokenize-and-clean-text (rest (assoc :text (get-json-object-from-file *corpus-json-article-texts* 2))))
;;(assoc :--id  (get-json-object-from-file *test-in*))

(defun remove-stopwords (string)
  (let ((tokens (split-sequence:split-sequence #\Space string :remove-empty-subseqs t)))
    (remove-if #'stopword-p tokens)))

(defun stopword-p (string-word &key (stopword-hash-table *stopwords*))
  "returns true if string-word is found in the list of stopwords."
  (gethash string-word stopword-hash-table))

;; (mapcar #'remove-stopwords (sentence-tokenize-and-clean-text (rest (assoc :text (get-json-object-from-file *corpus-json-article-texts* 3)))))

(defun remove-punctuation-not-numbers (string)
  (let ((string string))
    (setf string (cl-ppcre:regex-replace-all " [-*_.,;:`!?#-()\\\"] " string " "))
    (setf string (cl-ppcre:regex-replace-all " [*_.,;:`!?#-()\\\"]" string " "))
    (setf string (cl-ppcre:regex-replace-all "[*_.,;:`!?#-()\\\"] " string " "))))

;;(remove-punctuation-not-numbers "-PRON- note that one of -PRON- executive be show - horror - shake hand with the secretary general .  ")

(defun get-article-text (json-object)
  "Returns the text of an article as a string."
  (rest (assoc :text json-object)))

;; (get-article-text (get-json-object-from-file *corpus-json-no-html*))

(defun remove-html-field (json-object)
  "Removes the html field of the json-object"
  (remove (assoc :html json-object) json-object))

;; (remove-html-field (get-json-object-from-file *corpus-json*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating subcorpora                                                            ;;
;; Read-Do-Write only: never collect and do not read the whole corpus into memory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sentence-tokenize-and-clean-json-objects-in-parallel (input-corpus output-corpus)
  (process-corpus :function #'extract-sentences-from-json-object-one-per-line
                  :inputfile input-corpus
                  :outputfile output-corpus
                  :number-of-threads 8
                  :number-of-lines-per-thread 5))

(defparameter *test-in* (babel-pathname :directory '(:up "Corpora" "Guardian" )
                                            :name "test-in"
                                            :type "json"))

;;to make test file::
#|
(with-open-file (outputstream *test-in* :direction :output  :if-does-not-exist :create)
  (loop for object in (get-json-objects-from-file  *corpus-json-article-texts-withcomments* 5)
        do (format outputstream "~a~%" (encode-json-alist-to-string object)))) |#

(defparameter *test-out* (babel-pathname :directory '(:up "Corpora" "Guardian")
                                            :name "test-out"
                                            :type "json"))


;; (sentence-tokenize-and-clean-json-objects-in-parallel *test-in* *test-out*)
;; for Mac Pro:
;; (sentence-tokenize-and-clean-json-objects-in-parallel *corpus-json-article-texts-withcomments* *corpus-json-article-sentences-all*)

(defun add-article-id (input-corpus output-corpus)
  ""
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (progn
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (let ((json-object (decode-json-from-string line)))
                   (format outputstream "~a~%" (encode-json-alist-to-string
                                                (push (cons :id object-number) json-object))))
                 (incf object-number))))))

;; (add-article-id *corpus-json-article-texts* *corpus-json-article-texts-with-id)

(defun create-corpus-of-original-article-sentences (input-corpus output-corpus)
  "Takes an inputfile with json objects with articles, writes the individual article sentences of each object to output file as json object.
   Entries containing no article text are ignored."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            with sentence-id = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
            (incf object-number)
            (let ((sentences (cdr (assoc :sentences  (decode-json-from-string line))))
                  (article-id  (assoc :article-id  (decode-json-from-string line))))
              (when sentences
                (loop with sentence-index = 1
                      for sentence in sentences
                      do
                      (format outputstream "~a~%" (encode-json-alist-to-string
                                                   `(("sentence-id" . ,sentence-id)
                                                     ("sentence" . ,(format nil "~a" sentence))
                                                     ,article-id
                                                     ("sentence-index" . ,sentence-index))))
                      (incf sentence-id)
                      (incf sentence-index))))))))



(defun create-corpus-of-original-article-sentences-and-comment-sentences (input-corpus output-corpus)
  "Takes an inputfile with json objects with articles, writes the individual article sentences of each object to output file as json object.
   Entries containing no article text are ignored."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            with sentence-id = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
            (incf object-number)
            (let ((sentences (cdr (assoc :sentences  (decode-json-from-string line))))
                  (article-id (assoc :article-id  (decode-json-from-string line)))
                  (comments (cdr (assoc :comments  (decode-json-from-string line)))))
               (when sentences
                 (if comments
                   (let ((sentence-index 1))
                     (loop for sentence in sentences
                           do (format outputstream "~a~%" (encode-json-alist-to-string
                                                           `(("sentence-id" . ,sentence-id)
                                                             ("sentence" . ,(format nil "~a" sentence))
                                                             ,article-id
                                                             ("sentence-index" . ,sentence-index))))
                           (incf sentence-id)
                           (incf sentence-index))
                     (loop for comment in comments
                           for comment-sentences = (cdr (assoc :sentences comment))
                           for comment-author = (cdr (assoc :comment-author comment))
                         ;  for comment-id = (cdr (assoc :comment-id comment))
                           for comment-time = (cdr (assoc :comment-time comment))
                           do (loop for comment-sentence in comment-sentences
                                    do (format outputstream "~a~%" (encode-json-alist-to-string
                                                                    `(("sentence-id" . ,sentence-id)
                                                                      ("sentence" . ,(format nil "~a" comment-sentence))
                                                                      ,article-id
                                                                      ("sentence-index" . ,sentence-index)
                                                                      ("author" . ,comment-author)
                                                                      ("time-stamp" . ,comment-time))))
                                    (incf sentence-id)
                                    (incf sentence-index))))
                           
                   (loop with sentence-index = 1
                         for sentence in sentences
                         do
                         (format outputstream "~a~%" (encode-json-alist-to-string
                                                      `(("sentence-id" . ,sentence-id)
                                                        ("sentence" . ,(format nil "~a" sentence))
                                                        ,article-id
                                                        ("sentence-index" . ,sentence-index))))
                         (incf sentence-id)
                         (incf sentence-index)))))))))


;;(create-corpus-of-original-article-sentences-and-comment-sentences *corpus-json-article-sentences-per-article* *corpus-json-article-sentences*)
;; TO RUN ON MAC PRO
;;(create-corpus-of-original-article-sentences-and-comment-sentences *corpus-json-article-sentences-withcomments* *corpus-json-article-sentences*)
  
(defun create-corpus-of-stop-word-filtered-sentences (input-corpus output-corpus)
  "Takes an inputfile with json objects, writes the article-text of each object to output file as json object.
   Entries containing no article text are ignored."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (progn
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)
                 (let ((sentences (cdr (assoc :sentences  (decode-json-from-string line)))))
                   (if sentences
                     (loop for sentence in sentences
                           do
                           (format outputstream "~a~%" (encode-json-to-string
                                                        `(("sentence" . ,(format nil "~{~a~^ ~}" (remove-stopwords sentence)))))))
                     )))))))

;; (create-corpus-of-stop-word-filtered-sentences *corpus-json-article-sentces-lemmatized* *corpus-json-article-sentences-filtered*)

(defun create-corpus-of-article-texts (input-corpus output-corpus)
  "Takes an inputfile with json objects, writes the article-text of each object to output file as json object.
   Entries containing no article text are ignored."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (progn
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)
                 (let ((article-text (assoc :text  (decode-json-from-string line)))
                       (article-id (assoc :--id  (decode-json-from-string line))))
                   (when article-text 
                     (format outputstream "~a~%" (encode-json-alist-to-string
                                                  (list article-id article-text))))))))))

;; (create-corpus-of-article-texts *corpus-json-no-html* *corpus-json-article-texts*)
;; (create-corpus-of-article-texts *test-in* *test-out*)

(defun create-corpus-of-article-text-and-comments (input-corpus output-corpus)
  "Takes an inputfile with json objects, writes the article-text of each object to output file as json object.
   Entries containing no article text are ignored."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (progn
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)
                 (let ((article-text (assoc :text  (decode-json-from-string line)))
                       (article-id (assoc :--id  (decode-json-from-string line)))
                       (article-comments (assoc :comments  (decode-json-from-string line))))
                   (when article-text
                     (if article-comments
                       (format outputstream "~a~%" (encode-json-alist-to-string
                                                    (list article-id article-text article-comments)))
                       (format outputstream "~a~%" (encode-json-alist-to-string
                                                    (list article-id article-text)))))))))))

;; (create-corpus-of-article-text-and-comments *corpus-json-no-html* *corpus-json-article-texts-withcomments*)

(defun create-corpus-without-html-field (input-corpus output-corpus)
  "Create a corpus with the same json object, but without the html field."
  (with-open-file (inputstream input-corpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream output-corpus :direction :output :if-exists :error :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (progn
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)
                 (format outputstream "~a~%" (encode-json-to-string (remove-html (decode-json-from-string line)))))))))

;; (create-corpus-without-html-field *corpus-json* *corpus-json-no-html*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For testing mainly, use only for small numbers of articles ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-json-object-from-file (file-with-json-objects &optional (n 1))
  "Returns the nth json object in the file."
  (last-elt (get-json-objects-from-file file-with-json-objects n)))

;; (get-json-object-from-file *corpus-json-article-texts-withcomments* 5)

(defun get-json-objects-from-file (file-with-json-objects &optional (number-or-all 'all))
  "Returns a list containing the first n json objects of the file. Never use all on large files."
  (assert (or (numberp number-or-all)
              (eql number-or-all 'all)))
  (with-open-file (inputstream file-with-json-objects :direction :input :if-does-not-exist :error)
    (if (numberp number-or-all)
      ;; A number was specified
      (loop for i from 1 upto number-or-all
            for line = (read-line inputstream nil)
            when line
            collect (decode-json-from-string line))
      ;; No number was specified, get them all
      (loop
       for line = (read-line inputstream nil)
       until (eq line nil)
       collect line))))


;;;;;;;;;;;;;;;;;;;;
;; Counting words ;;
;;;;;;;;;;;;;;;;;;;;


(defun create-token-frequency-distribution (inputcorpus outputfile)
  "Returns a frequency distribution for the tokens in the given text in the form of a hash table."
  (let ((frequency-hash-table (make-hash-table :test #'equal))
        (frequency-list nil))
    (with-open-file (inputstream inputcorpus :direction :input :if-does-not-exist :error)
      (loop for line = (read-line inputstream nil)
            until (eq line nil)
            when line
            do (loop for token in (split-sequence:split-sequence #\Space (cdr (assoc :sentence (decode-json-from-string line))) :remove-empty-subseqs t)
                     do (if (gethash token frequency-hash-table)
                          (incf (gethash token frequency-hash-table))
                          (setf (gethash token frequency-hash-table) 1)))))
    (maphash (lambda (k v) (push (cons k v) frequency-list)) frequency-hash-table)
        (with-open-file (outputstream outputfile :direction :output :if-exists :error :if-does-not-exist :create)
            (loop for (key . value) in (sort frequency-list #'> :key #'cdr)
                  do (format outputstream "~a~%" (encode-json-alist-to-string `((,key . ,value))))))))

;; (create-token-frequency-distribution *corpus-json-article-sentences-filtered* *frequency-count-file*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline for creating subcorpus with sentences with particular search term ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-sentences-with-search-term-in-parallel (input-corpus output-corpus keyphrase)
  "Extract all sentences with a given keyphrase and write them to a new file in parallel"
  (process-corpus :function #'extract-sentences-with-search-term
                  :function-kwargs (list :keyphrase keyphrase)
                  :inputfile input-corpus
                  :outputfile output-corpus
                  :number-of-threads 8
                  :number-of-lines-per-thread 5))

(defun extract-sentences-with-search-term (json-object &key keyphrase)
  (let* ((decoded-object (restart-case (decode-json-from-string json-object)
                           ;; if decode-json raises an error (invalid char)
                           ;; remove the char and retry
                           ;; if another type of error is raised, skip this entry
                           (skip-entry () nil)
                           (fix-and-retry (fixed-object)
                             (extract-sentences-with-search-term fixed-object :keyphrase keyphrase))))
         (sentence (when decoded-object (cdr (assoc :sentence decoded-object)))))
    (when (search keyphrase sentence :test #'equalp)
      json-object)))

(defun write-sentences-with-keyphrase-to-file (inputcorpus outputfile keyphrase)
  "Find all sentences with given keyphrase in a sentence-based json
file and write them to a new json file."
  (with-open-file (inputstream inputcorpus :direction :input :if-does-not-exist :error)
    (with-open-file (outputstream outputfile :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (let ((sentence (cdr (assoc :sentence  (decode-json-from-string line)))))
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)
                 (when (search keyphrase sentence :test #'equalp)
                   (format outputstream "~a~%" line)))))))

(defun sentence-word-embeddings-in-parallel (input-corpus output-corpus keyphrase &key (source 'glove) (filter-stopwords? nil))
  (process-corpus :function #'sentence-word-embeddings
                  :function-kwargs (list :keyphrase keyphrase
                                         :source source
                                         :filter-stopwords? filter-stopwords?)
                  :inputfile input-corpus
                  :outputfile output-corpus
                  :number-of-threads 8
                  :number-of-lines-per-thread 5))

(defun sentence-word-embeddings (json-object &key keyphrase (source 'glove) (filter-stopwords? nil))
  (when (> (length json-object) 0) ;; to skip empty lines in the input
    (let* ((decoded-object (decode-json-from-string json-object))
           (sentence (assoc :sentence decoded-object))
           (sentence-without-keyphrase (remove-spurious-spaces (string-replace (cdr sentence) keyphrase "")))
           (clean-sentence (if filter-stopwords?
                             (list-of-strings->string (remove-stopwords sentence-without-keyphrase))
                             sentence-without-keyphrase))
           (word-embeddings (get-word-embeddings clean-sentence :source source))
           (words-without-glove-vector
            (remove nil (mapcar #'(lambda (token+vector)
                                    (when (find 0 (second token+vector) :test #'=)
                                      (first token+vector))) word-embeddings)))
           (word-vectors (remove nil (mapcar #'(lambda (token+vector)
                                                 (unless (find 0 (second token+vector) :test #'=)
                                                   (second token+vector))) word-embeddings)))
           (article-id (assoc :article-id decoded-object))
           (sentence-index (assoc :sentence-index decoded-object))
           (sentence-id (assoc :sentence-id decoded-object))
           sentence-vector)
      (when word-vectors
        (setf sentence-vector (mapcar #'(lambda (number) ;;normalise
                                          (/ number (length word-vectors)))
                                      (sum-list-of-vectors (mapcar #'(lambda (vector)
                                                                       (mapcar #'exp vector))
                                                                   word-vectors))))
        (format nil "~a~%" (encode-json-alist-to-string
                            `(,sentence-id
                              ("sentence" . ,(cdr sentence))
                              ("sentence-vector" . ,sentence-vector)
                              ("words-without-glove-vector" ,@words-without-glove-vector)
                              ,article-id
                              ,sentence-index)))))))
  

(defun add-sentence-word-embeddings (inputcorpus outputfile keyphrase &key (source 'glove) (filter-stopwords? nil))
  "Read in sentence corpus and calculate word embeddings for
individual words + sum these for every sentence (+normalize). Filter
keyphrase from sentences before you make the word embeddings"
  (with-open-file (inputstream inputcorpus :direction :input :if-does-not-exist :create)
    (with-open-file (outputstream outputfile :direction :output :if-exists :error :if-does-not-exist :create)
      (format t "Calculating sentence embeddings... ~%")
      (loop with object-number = 1
            for line = (read-line inputstream nil)
            until (eq line nil)
            do (let* ((sentence (assoc :sentence (decode-json-from-string line)))
                      (sentence-vector nil)
                      (sentence-without-keyphrase (remove-spurious-spaces (string-replace (cdr sentence) keyphrase "")))
                      (clean-sentence (if filter-stopwords?
                                        (list-of-strings->string (remove-stopwords sentence-without-keyphrase))
                                        sentence-without-keyphrase))
                      (word-embeddings (get-word-embeddings clean-sentence :source source))
                      (words-without-glove-vector
                       (remove nil (mapcar #'(lambda (token+vector)
                                               (when (find 0 (second token+vector) :test #'=)
                                                 (first token+vector))) word-embeddings)))
                      (word-vectors (remove nil (mapcar #'(lambda (token+vector)
                                                            (unless (find 0 (second token+vector) :test #'=)
                                                              (second token+vector))) word-embeddings)))
                      (article-id (assoc :article-id  (decode-json-from-string line)))
                      (sentence-index (assoc :sentence-index  (decode-json-from-string line)))
                      (sentence-id (assoc :sentence-id  (decode-json-from-string line))))                 
                 (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                 (incf object-number)

                 (when word-vectors
                   (setf sentence-vector (mapcar #'(lambda (number) ;;normalise
                                                     (/ number (length word-vectors)))
                                                 (sum-list-of-vectors (mapcar #'(lambda (vector)
                                                                                  (mapcar #'exp vector))
                                                                              word-vectors))))) ;;sum in linear space
                 
                 (format outputstream "~a~%" (encode-json-to-string
                                              `(,sentence-id
                                                ("sentence" . ,(cdr sentence))
                                                ("sentence-vector" . ,sentence-vector)
                                                ("words-without-glove-vector" ,@words-without-glove-vector)
                                                 ,article-id
                                                 ,sentence-index))))))))

(defun create-sentence-distance-matrix (inputcorpus outputfile)
  "Take an input file with sentences and their vectors and calculate
the cosine distance between all pairs. Only fill the upper part of the
matrix."
  (let ((sentence-vectors nil))
   (with-open-file (inputstream inputcorpus :direction :input :if-does-not-exist :create)
     (with-open-file (outputstream outputfile :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format t "Creating the cosine distance matrix... ~%")
      ;;1) read sentence vectors in memory:
      (loop for line = (read-line inputstream nil)
            until (eq line nil)
            do (let ((sentence-vector (cdr (assoc :sentence-vector (decode-json-from-string line))))
                     (sentence-id (cdr (assoc :sentence-id (decode-json-from-string line)))))
                 (setf sentence-vectors (append (list (cons sentence-id sentence-vector))
                                                sentence-vectors))))
      (setf sentence-vectors (reverse sentence-vectors))

      ;;2) calculate cosine similarity and write matrix out:
      (loop with object-number = 1
            for (sentence-id . sentence-vector) in sentence-vectors
            for i from 1
            for distances/ids = (loop for (other-sentence-id . other-sentence-vector) in sentence-vectors
                                      for j from 1
                                      for cosine-distance = ;;(if (<= i j)
                                                             ;; 'N/A
                                                              (cosine-similarity sentence-vector other-sentence-vector)
                                                             ;; )
                                      collect (cons other-sentence-id cosine-distance))
            do (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
            (incf object-number)
            (if (= i 1)
              (progn (format outputstream " ,~{~a~^, ~} ~%" (mapcar #'first distances/ids)) ;;write header
                (format outputstream  "~a,~{~a~^, ~} ~%"sentence-id (mapcar #'rest distances/ids)))
              (format outputstream "~a,~{~a~^, ~} ~%" sentence-id (mapcar #'rest distances/ids))))))))


(defun keyphrase-sentences->distance-matrix (inputcorpus outputfile keyphrase &key (full-corpus? t) (filter-stopwords? nil))
  "Takes a sentence corpus (json) as input and searches for all
sentences that contain a given keyphrase, calculates their sentence
embeddings (by summing + normalising their glove vectors) and computes
the cosine distance matrix. If the keyword full-corpus? is set to NIL
the inputcorpus will not be filtered on keyphrase."
  (let ((keyphrase-corpus-file (babel-pathname :directory `(:up "Corpora" "Guardian" ,keyphrase)
                                               :name (string-append keyphrase "-sentences")
                                               :type "json"))
        (keyphrase-corpus-with-word-embeddings-file
         (babel-pathname :directory `(:up "Corpora" "Guardian" ,keyphrase)
                         :name (string-append keyphrase "-vectors")
                         :type "json")))

    ;;create corpus with keyphrase sentences
    (if full-corpus?
      (write-sentences-with-keyphrase-to-file inputcorpus keyphrase-corpus-file keyphrase)
      (setf keyphrase-corpus-file inputcorpus))
    ;;calculate sentence embeddings
    (add-sentence-word-embeddings keyphrase-corpus-file keyphrase-corpus-with-word-embeddings-file keyphrase :filter-stopwords? filter-stopwords?)
    ;;write distance matrix
    (create-sentence-distance-matrix keyphrase-corpus-with-word-embeddings-file outputfile)))

(defparameter *turnbull-sentences* (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                                         :name "turnbull-sentences"
                                         :type "json"))

(defparameter *distance-matrix-full-sentences* (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                                                :name "distance-matrix-full-sentences"
                                                :type "csv"))

(defparameter *distance-matrix-no-stopwords* (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                                                :name "distance-matrix-no-stopwords"
                                                :type "csv"))



;(add-sentence-word-embeddings *test-in* *test-out* "turnbull")

;;(keyphrase-sentences->distance-matrix *turnbull-sentences* *distance-matrix-no-stopwords* "turnbull" :full-corpus? nil :filter-stopwords? nil)


(defun get-n-closest-sentence-ids (target-sentence-id distance-matrix-file &optional (n 10))
  ""
  (with-open-file (inputstream distance-matrix-file :direction :input :if-does-not-exist :error)
    (loop with headers = nil
          for line = (read-line inputstream nil)
          for i from 0
          until (eq line nil)
          if (= i 0)
          do (setf headers (split-sequence:split-sequence #\, line))
          else do (let* ((row-fields (split-sequence:split-sequence #\, line))
                         (sentence-id (read-from-string (first row-fields))))
                    (when (= target-sentence-id sentence-id)
                      (let ((similarity/index
                             (loop for i from 1
                                   for field in (rest row-fields)
                                   for header = (read-from-string (nth i headers))
                                   unless (= header sentence-id)
                                   collect (list header
                                                 (read-from-string field)))))
                        (return-from get-n-closest-sentence-ids
                                (subseq (sort similarity/index #'> :key #'second)
                                        0 n))))))))

(defun get-n-closest-sentences (target-sentence-id distance-matrix-file
                                                   sentences-file &optional
                                                   (output-file nil) (n 10))
  (let ((closest-id/sim-list (get-n-closest-sentence-ids target-sentence-id
                                                         distance-matrix-file
                                                         n))
        (closest-sentences nil))
    
    (with-open-file (inputstream sentences-file :direction :input :if-does-not-exist :create)
      (loop with found-sentences = 0
            for line = (read-line inputstream nil)
            until (or (= found-sentences n)
                      (eq line nil))
            do (let ((sentence-id (cdr (assoc :sentence-id (decode-json-from-string line)))))
                 (when (find sentence-id closest-id/sim-list :key #'first)
                   (let ((sentence (cdr (assoc :sentence (decode-json-from-string line)))))
                     (incf found-sentences)
                     (setf closest-sentences
                           (append closest-sentences
                                   (list (list sentence-id
                                               sentence
                                               (second (find sentence-id closest-id/sim-list :key #'first)))))))))))

    (setf closest-sentences
          (sort closest-sentences #'> :key #'third))
    (when output-file
      (with-open-file (outputstream output-file :direction :output :if-does-not-exist :create)
        (loop for (id sentence sim-score) in closest-sentences
              do (format outputstream  "~a; ~a; ~a; ~%" id sentence sim-score ))))
        
    closest-sentences))
                 
(defparameter *sentence-ranking-no-stopwords* (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                                                :name "sentence-ranking-no-stopwords"
                                                :type "csv"))

(defparameter *sentence-ranking-full-sentences* (babel-pathname :directory '(:up "Corpora" "Guardian" "turnbull")
                                                :name "sentence-ranking-full-sentences"
                                                :type "csv"))

;;(get-n-closest-sentences 6500 *distance-matrix-no-stopwords* *turnbull-sentences* *sentence-ranking-no-stopwords* 50)

;;(get-n-closest-sentences 6500 *distance-matrix-full-sentences* *turnbull-sentences* *sentence-ranking-full-sentences* 50)

;;(nlp-tools::get-phrase-similarity  "But Turnbull has two problems." "Turnbull also insisted the Coalitionâs Direct Action plan could meet the target Australia will take to the United Nations conference in Paris in December â to reduce emissions by between 26% and 28% of 2005 levels by 2030.He said an ETS âused to be Coalition policy but it is not any moreâ, saying an ETS was âno more than one mechanism to reduce emissions, it is a means to an end ...")

;;(get-penelope-sentence-tokens "the same charge levelled by the Coalition minister Malcolm Turnbull when he explained in 2011 that continuing to use a big government taxpayer-funded scheme to reduce emissions in the long term would “become a very expensive charge on the budget in the years ahead.Pre-election promises by the Coalition would consider higher targets than 5% were made by Hunt, in an article for the Australian Financial Review, in which he said “the Coalition is committed to a target of a 5% reduction in emissions and the conditions for extending that target further, based on international action” and in a speech to the Grattan Institute thinktank in July, when Hunt said “we also accept, and we gave support to the government for the targets, not just the 5% but also the conditions for change ...")









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old code...                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun lookup-lemmatized-sentences-and-write-to-file (unlemmatized-sentences reference-corpus lemmatized-sentences)
  "Lookup the lemmatized versions of the keyphrase sentence and write them to a new file"
  (let ((sentence-ids nil))
    (with-open-file (inputstream unlemmatized-sentences :direction :input :if-does-not-exist :error)
      (loop for line = (read-line inputstream nil)
            until (eq line nil)
            do (let ((sentence-id (cdr (assoc :sentence-id  (decode-json-from-string line)))))
                 (setf sentence-ids (push sentence-id sentence-ids)))))

    (with-open-file (inputstream reference-corpus :direction :input :if-does-not-exist :error)
      (with-open-file (outputstream lemmatized-sentences :direction :output :if-exists :error :if-does-not-exist :create)
        (loop with object-number = 1
              for line = (read-line inputstream nil)
              until (eq line nil)
              do (let ((sentence-id (cdr (assoc :sentence-id  (decode-json-from-string line)))))
                   (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                   (incf object-number)
                   (when (find sentence-id sentence-ids)
                     (format outputstream "~a~%" line))))))))


#|(lookup-lemmatized-sentences-and-write-to-file *corpus-json-article-sentences-with-turnbull*
                                               *corpus-json-article-sentences-lemmatized*
                                               *corpus-json-article-sentences-with-turnbull-lemmatized*)
                                               
|#

(defun decode-json-as-hashtable (json-file)
  (let ((frequency-counts (make-hash-table :test #'equal)))
    (with-open-file (inputstream json-file :direction :input :if-does-not-exist :error)
      (loop for line = (read-line inputstream nil)
            until (eq line nil)
            do (let* ((decoded-line (first (decode-json-from-string line)))
                      (token (downcase (mkstr (first decoded-line))))
                      (freq (rest decoded-line)))
            (setf (gethash token frequency-counts) freq))))
    frequency-counts))

;(defparameter *frequency-counts* (decode-json-as-hashtable *frequency-count-file*))
  
(defun remove-spurious-spaces (sentence-as-string)
  (let ((list-representation (split-sequence:split-sequence #\Space sentence-as-string :remove-empty-subseqs t)))
    (format nil "~{~a~^ ~}" list-representation)))

;(remove-spurious-spaces (string-replace "i saw turnbull last week" "turnbull" ""))

(defun apply-frequency-filter-to-sentences (inputcorpus outputfile frequency-counts &key (min-freq 30) (max-freq 10000))
  "Only keeps the lemmas that have a frequency that is smaller than max-freq and higher than min-freq"
    ;;do we need to filter the keyphrase as well??
    (with-open-file (inputstream inputcorpus :direction :input :if-does-not-exist :error)
      (with-open-file (outputstream outputfile :direction :output :if-exists :error :if-does-not-exist :create)
        (loop with object-number = 1
              for line = (read-line inputstream nil)
              until (eq line nil)
              do (let ((filtered-sentence nil)
                       (sentence-as-list
                        (split-sequence:split-sequence #\Space (cdr (assoc :sentence  (decode-json-from-string line)))))
                       (article-id (assoc :article-id  (decode-json-from-string line)))
                       (sentence-index (assoc :sentence-index  (decode-json-from-string line)))
                       (sentence-id (assoc :sentence-id  (decode-json-from-string line))))
                   (when (= 0 (mod object-number 50)) (format t "~a~%" object-number))
                   (incf object-number)
                   (setf filtered-sentence
                         (loop for word in sentence-as-list
                               for word-freq = (gethash word frequency-counts)
                               when (and word-freq
                                         (> word-freq min-freq)
                                         (< word-freq max-freq))
                               collect word))
                   (format outputstream "~a~%" (encode-json-to-string
                                                `(,sentence-id
                                                  ("sentence" . ,(format nil "~{~a~^ ~}" filtered-sentence))
                                                  ,article-id
                                                  ,sentence-index))))))))

#|(apply-frequency-filter-to-sentences *corpus-json-article-sentences-with-turnbull-lemmatized*
                                     *corpus-json-article-sentences-with-turnbull-lemmatized-filtered*
                                     *frequency-counts*)
 |#

(defun find-sentence-in-json-file (json-file target-sentence-id)
  "Returns the value of the sentence field in the json object that corresponds to the target-sentence-id."
  (with-open-file (inputstream json-file :direction :input :if-does-not-exist :error)
    (loop for line = (read-line inputstream nil)
          until (equal line nil)
          do (let ((sentence-id (cdr (assoc :sentence-id  (decode-json-from-string line)))))
               (when (equal sentence-id target-sentence-id)
                 (return (cdr (assoc :sentence (decode-json-from-string line)))))))))

;;(find-sentence-in-json-file *test-in* 6460)




;(add-sentence-word-embeddings *test-in* *test-out* *corpus-json-article-sentences-with-turnbull*)
#|(add-sentence-word-embeddings *corpus-json-article-sentences-with-turnbull-lemmatized-filtered*
                              *corpus-json-article-sentences-with-turnbull-with-vectors*
                              *corpus-json-article-sentences-with-turnbull* :source 'glove)
|#

#|
(cosine-similarity 
 (cdr (assoc :sentence-vector (get-json-object-from-file *test-out* 1)))
 (cdr (assoc :sentence-vector (get-json-object-from-file *test-out* 7))))
|#




;(create-sentence-distance-matrix *test-in* *test-out*)






#|
(defparameter *vub-api-key* "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiVlVCIiwicGFzc3dvcmQiOiJQZW5lbG9wZVZVQiJ9.DrxOBckzLPg4gsn8hasGc4ii8Bt8SAwptgpEPMqiHOM")
(defparameter *upmc-api-key* "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiVVBNQyIsInBhc3N3b3JkIjoiUGVuZWxvcGVVUE1DIn0.ToxJ_5D-XGS-NlGiAcvcR0rIkcERBqCR_YvXOn3fuyU")
  
(defun get-penelope-documents (&key (start-date "2002-02-15T10:53:53.000Z")
                                 end-date (collection "GuardianArticles")
                                 (api-key *vub-api-key*) (order 1) (limit 1))
  "Call the penelope server to get the articles from The Guardian climate change corpus."
  
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/data/documents"
                    (cl-json-shell:encode-json-to-string-for-shell `((:collection . ,collection)
                                              (:api_key . ,api-key)
                                              (:start_date . ,start-date)
                                              ,@(when end-date
                                                  `((:end-date . ,end-date)))
                                              (:order . ,order)
                                              (:limit . ,limit))))))
     server-result))

;;(print (get-penelope-documents :api-key *vub-api-key*))


;(time (get-penelope-documents :order -1 :limit 1000))

;(defparameter *guardian-climate-corpus* (get-penelope-documents :order -1 :limit 1))

;(defparameter *guardian-climate-corpus* (get-penelope-documents :order -1 :limit 1000))

;(defparameter *test-entry* (first *guardian-climate-corpus*))
;;(length *guardian-climate-corpus*)

;(defparameter *all-texts* (mapcar #'(lambda(entry) (rest (assoc :text entry)))  *guardian-climate-corpus*))
;(defparameter *all-authors* (mapcar #'(lambda(entry) (rest (assoc :authors entry)))  *guardian-climate-corpus*))

;;rewriting functions that were in mongo-interface.lisp
;;------------------------------------------------------

(defun get-all-article-texts (&key (corpus *guardian-climate-corpus*))
  "Finds and returns the strings associated with the :text field in the corpus in a list."
  (mapcar #'(lambda(entry) (rest (assoc :text entry))) corpus)
  )

;;(defparameter *all-sentences* (mappend #'get-penelope-sentence-tokens (get-all-article-texts)))

(defun find-sentences-with-keyword (keyword &key (corpus *guardian-climate-corpus*))
  (let ((all-sentences (mappend #'get-penelope-sentence-tokens (get-all-article-texts :corpus corpus)))
        (sentences-with-keyword nil))
    (dolist (sentence all-sentences)
      (when (search keyword sentence)
        (push sentence sentences-with-keyword)))
    (when sentences-with-keyword
      (reverse sentences-with-keyword))))

;;(find-sentences-with-keyword "China")


(defun get-db-entries-by-author (author &key (corpus *guardian-climate-corpus*))
  "Finds all entries in the DB with the a author."
  (unless (stringp author)
    (error "The author's name should be provided as a string. You entered a ~a instead" (type-of author)))

  (let (articles-by-author)
    (dolist (article corpus)
      (let ((article-author (assoc :authors article)))
        (when (and article-author
                   (member author (rest article-author) :test #'string=))
          (setf articles-by-author (push article articles-by-author)))))
    articles-by-author))

;;(get-db-entries-by-author "Naomi Klein")

(defun get-articles-by-author (author &key (corpus *guardian-climate-corpus*))
  (let ((db-entries-for-author (get-db-entries-by-author author :corpus corpus)))
    (get-all-article-texts :corpus db-entries-for-author)))
  

;;(get-articles-by-author "Naomi Klein")
;;(get-articles-by-author "Stuart Clark")

(defun string-capitalize-first-word (string)
  (let* ((first-word-length (length (first (split-sequence:split-sequence #\Space string))))
         (first-word (subseq string 0 first-word-length)))
    (string-append (string-capitalize first-word)
                   (subseq string first-word-length))))

;;(string-capitalize-first-word "climate change")

(defun get-db-entries-by-tag (tag &key (corpus *guardian-climate-corpus*))
  (unless (stringp tag)
    (error "The keyword should be provided as a string. You entered a ~a instead" (type-of tag)))
  
    (let (articles-by-keyword)
    (dolist (article corpus)
      (let ((article-keywords (assoc :ARTICLE--TAGS article)))
        (when (and article-keywords
                   (member (string-capitalize-first-word tag) (rest article-keywords) :test #'string=))
          (setf articles-by-keyword (push article articles-by-keyword)))))
    articles-by-keyword))

;;(get-db-entries-by-tag "climate change")

(defun get-articles-by-tag (tag &key (corpus *guardian-climate-corpus*))
  (let ((db-entries-for-keyword (get-db-entries-by-keyword tag :corpus corpus)))
    (get-all-article-texts :corpus db-entries-for-keyword)))

;;(get-articles-by-tag "Environment")


                                



;
 
;curl -X POST "https://www.fcg-net.org/penelope/data/documents" -H  "accept: application/json" -H  "Content-Type: application/json" -d "{  \"collection\": \"GuardianArticles\",  \"api_key\": \"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiVVBNQyIsInBhc3N3b3JkIjoiUGVuZWxvcGVVUE1DIn0.ToxJ_5D-XGS-NlGiAcvcR0rIkcERBqCR_YvXOn3fuyU\",  \"start_date\": \"2013-01-13T10:53:53.000Z\", \"order\": 1,  \"limit\": 1, \"filter\": [\"comments\"]}"


|#

