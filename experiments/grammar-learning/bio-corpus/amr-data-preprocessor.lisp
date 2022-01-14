
(ql:quickload :cl-json)
(ql:quickload :split-sequence)

(defparameter *amr-source-file*
  (parse-namestring "../babel-corpora/bio-corpus-amr/source/amr-release-bio-v3.0.txt"))

(defparameter *json-output-file*
  (parse-namestring "../babel-corpora/bio-corpus-amr/pre-processed/bio-corpus-amr.json"))



(defun extract-by-header (line header)
  "extract sentences that start with # ::snt"
  (when (and
         (> (length line) (length header))
         (string-equal (subseq line 0 (length header)) header))
    (subseq line (+ 1 (length header)) (length line))))
     

(defun read-amr-file (amr-source-file)
    (with-open-file (stream amr-source-file)
      (loop repeat 3
                do (read-line stream nil nil))
      (loop for line = (read-line stream nil)          
            for sentence = (extract-by-header line "# ::snt")
            for id = (extract-by-header line "# ::id")
            for save-date = (extract-by-header line "# ::save-date")
            while line
            when (not (string-equal "" line))
            if sentence
            collect sentence into sentences
            else
            if id
            collect id into ids
            and append (list (format nil "~{~a~}" partial-amr-meaning)) into amr-meanings
            and do (setf partial-amr-meaning nil)
            else
            if save-date
            collect save-date into save-dates
            else
            append (list line) into partial-amr-meaning
            finally (return (values ids sentences (cdr (append amr-meanings (list (format nil "~{~a~}" partial-amr-meaning)))) save-dates )))))



(defun split-with-subseq (seq subseq)
  "Recursively split a sequence by occurences of a subsequence."
  (let ((position (search subseq seq)))
    (if position
	(cons (subseq seq 0 position)
	      (split-with-subseq
	       (subseq seq (+ position (length subseq)))
	       subseq))
	(list seq))))

;; i'd love to do this with regex, but for now...
;; not using regex because idk how to do that in --script mode
(defun strip-html (s)
  "Remove all html tags from a string."
  (format nil "~{~A~}"
	  (mapcar (lambda (s)
	    (car (last (split-with-subseq s ">"))))
	  (split-with-subseq s "<"))))
                
(defun amr-to-json (amr-source-file json-output-file)
  (multiple-value-bind (meta-data-list sentences meanings save-dates) (read-amr-file amr-source-file)
    (with-open-file (output-stream json-output-file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (loop for id from 1
            for sentence in sentences
            for meta-data in meta-data-list
            for meaning in meanings
            for date in save-dates
            for meaning-string = (format nil "~{~a~^ ~}" (split-sequence:split-sequence #\Space meaning :remove-empty-subseqs t))
            for clean-sentence = (strip-html sentence)
            for clean-meaning = (strip-html meaning)
            for json = (cl-json:encode-json-to-string (list (cons "id" id)
                                                            (cons "utterance" clean-sentence)
                                                            (cons "meaning" clean-meaning)
                                                            (cons "meta-data" (list (cons "orig-id" meta-data) (cons "save-date" date)))
                                                            ))
            do (write-line json output-stream)))))
    
          
    

;(amr-to-json *amr-source-file* *json-output-file*)


