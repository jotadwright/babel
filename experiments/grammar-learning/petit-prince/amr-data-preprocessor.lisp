
(ql:quickload :cl-json)


(defparameter *amr-source-file*
  (parse-namestring "/Users/u0077062/Projects/babel-corpora/little-prince-amr/source/little-prince-amr.txt"))

(defparameter *json-output-file*
  (parse-namestring "/Users/u0077062/Projects/babel-corpora/little-prince-amr/pre-processed/little-prince-amr.json"))



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
            finally (return (values ids sentences (cdr (append amr-meanings (list (format nil "~{~a~}" partial-amr-meaning)))))))))
                            
                
(defun convert-petit-prince-to-json (amr-source-file json-output-file)
  (multiple-value-bind (meta-data-list sentences meanings) (read-amr-file amr-source-file)
    (with-open-file (output-stream json-output-file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (loop for id from 1
            for sentence in sentences
            for meta-data in meta-data-list
            for meaning in meanings
            for json = (cl-json:encode-json-to-string (list (cons "id" id)
                                                            (cons "utterance" sentence)
                                                            (cons "meaning" (format nil "~{~a~^ ~}" (split-sequence:split-sequence #\Space meaning :remove-empty-subseqs t)))
                                                            (cons "meta-data" meta-data)
                                                            ))
            do (write-line json output-stream)))))
    
          
    

;(convert-petit-prince-to-json *amr-source-file* *json-output-file*)


