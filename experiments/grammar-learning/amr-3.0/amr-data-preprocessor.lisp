
(ql:quickload :cl-json)

(defparameter *amr-source-files*
  (directory (merge-pathnames
              (make-pathname :directory '(:relative "amr-corpora" "amr_annotation_3.0" "data" "amrs" "unsplit"))
              cl-user:*babel-corpora*)))


(defparameter *json-output-file*
  (merge-pathnames
   (make-pathname :directory '(:relative "amr-corpora" "amr_annotation_3.0" "pre-processed")
                  :name "amr3" :type "json") cl-user:*babel-corpora*))
  

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
            finally (return (values ids sentences (append amr-meanings (list (format nil "~{~a~}" partial-amr-meaning))) save-dates )))))
                            
                
(defun convert-amr3-to-json (amr-source-files json-output-file)

  (with-open-file (output-stream json-output-file :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
    
    (loop for id from 1
        for amr-source-file in amr-source-files
        do  (multiple-value-bind (meta-data-list sentences meanings save-dates)
                (read-amr-file amr-source-file)
             
              (loop for sentence in sentences
                    for meta-data in meta-data-list
                    for meaning in meanings
                    for date in save-dates
                    for json = (cl-json:encode-json-to-string (list (cons "id" id)
                                                                    (cons "utterance" sentence)
                                                                    (cons "meaning" (format nil "~{~a~^ ~}"
                                                                                            (split-sequence:split-sequence #\Space meaning
                                                                                                                           :remove-empty-subseqs t)))
                                                                    (cons "meta-data" (list (cons "orig-id" meta-data)
                                                                                            (cons "save-date" date)
                                                                                            (cons "subcorpus" (file-namestring amr-source-file))))))
                    do (incf id)
                       (write-line json output-stream))))))
    
          
    

;(convert-amr3-to-json *amr-source-files* *json-output-file*)


