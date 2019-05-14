
(in-package :frame-extractor)
;(ql:quickload :frame-extractor)

(defun pie-comprehend-with-timeout (utterance timeout)
  (handler-case (trivial-timeout:with-timeout (timeout)
                  (pie-comprehend utterance
                                  :cxn-inventory *fcg-constructions*
                                  :silent t))
    (trivial-timeout:timeout-error (error)
      'timeout)))

(defun process-entry (sentence &key (time-out 5))
  "Sentence from The Guardian corpus, already preprocessed with lexical units."
  
      (format t ".")
      
      (let ((frame-set (pie-comprehend-with-timeout sentence time-out)))
        
        (unless (eql frame-set 'timeout)
        
          (when (and frame-set
                     (pie::entities frame-set))
            
            (let ((cause (frame-extractor::cause (first (pie::entities frame-set))))
                  (effect (frame-extractor::effect (first (pie::entities frame-set)))))
              
              (when (and cause effect)
                (encode-json-alist-to-string `(
                                               ("cause" . ,(or cause " "))
                                               ("effect" . ,(or effect " "))))))))))
#|
(defun process-entry (decoded-json-object &key (time-out 30))
  ""
  (let* ((sentence (cdr (assoc :sentence decoded-json-object)))
         (sentence-id (assoc :sentence-id  decoded-json-object))
         (article-id (assoc :article-id  decoded-json-object)))

    (when (cl-ppcre::scan-to-strings " [cC]aus.+" sentence) ;;only when sentence contains caus+
      
      (format t "~%[~a]:Comprehending \"~a\"~%" (cdr sentence-id) sentence)
      
      (let ((frame-set (pie-comprehend-with-timeout sentence time-out)))
        
        (if (eql frame-set 'timeout)
          
         `(,sentence-id
           ("sentence" . ,sentence)
           ("frame-elements" . nil)
           ,article-id
           ("timeout" . t))
        
          (when (and frame-set
                     (pie::entities frame-set))
            (let ((cause (frame-extractor::cause (first (pie::entities frame-set)))) ;;what if there are more?
                  (effect (frame-extractor::effect (first (pie::entities frame-set)))))
              (when (and cause effect)
                `(,sentence-id
                  ("sentence" . ,sentence)
                  ("frame-elements" (("frame-evoking-element" . ,(pie::frame-evoking-element (first (pie::entities frame-set))))
                                     ("cause" . ,cause)
                                     ("effect" . ,effect)))
                  ,article-id
                   ("timeout" . nil))))))))))



(defun process-entry-comments (decoded-json-object &key (time-out 30))
  ""
  (let* ((comment decoded-json-object)
         (sentences (get-penelope-sentence-tokens comment)))
    
    (format t "~%Comprehending \"~a\"~%"  sentences)
    
    (let* ((frame-sets (loop for sentence in sentences
                             for frame-set =  (pie-comprehend-with-timeout sentence time-out)
                             when frame-set
                             collect it))
           (aggregated-frame-set (loop for frame-set in frame-sets
                                       when (and (not (eql frame-set 'timeout))
                                                 (pie::entities frame-set))
                                       append (pie::entities frame-set))))
      
      `((:sentence . ,comment)
        (:frames . ,aggregated-frame-set)))))

|#

;;spacy api lokaal runnen!!

(defun evaluate-guardian-grammar-in-parallel (inputfile outputfile &optional (time-out 5))   
  (process-corpus :function #'process-entry
                  :function-kwargs (list :time-out time-out)
                  :inputfile inputfile
                  :outputfile outputfile
                  :number-of-threads 4
                  :number-of-lines-per-thread 2000))


(evaluate-guardian-grammar-in-parallel (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                       :name "causation_sentences_comments_2017"
                                                       :type "txt")
                                       (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                       :name "causation_frames_comments_2017"
                                                       :type "txt"))
 