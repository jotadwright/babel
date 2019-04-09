
(in-package :frame-extractor)
;(ql:quickload :frame-extractor)
(defun pie-comprehend-with-timeout (utterance timeout)
  (handler-case (trivial-timeout:with-timeout (timeout)
                  (pie-comprehend utterance
                                  :cxn-inventory *fcg-constructions*
                                  :silent t))
    (trivial-timeout:timeout-error (error)
      'timeout)))

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



(defun evaluate-guardian-grammar-in-parallel (inputfile outputfile &optional (time-out 30))   
  (json-stream-process :function #'process-entry-comments
                       :function-kwargs (list :time-out time-out)
                       :input-file inputfile
                       :output-file outputfile
                       :output-format :lines
                       :number-of-threads 1
                       :number-of-objects-per-thread 2000))

(evaluate-guardian-grammar-in-parallel "/Users/Shared/big-data-and-society/comments_x.txt"
                                       "/Users/Shared/big-data-and-society/commments_x_frames.json")

(evaluate-guardian-grammar-in-parallel "/Users/Shared/big-data-and-society/comments_y.txt"
                                       "/Users/Shared/big-data-and-society/commments_y_frames.json")

(activate-monitor trace-fcg)
(pie-comprehend "@Rob Honeycutt - You are compelled to follow me around and post empty knee-jerk rejections of everything I say, but you can't or you refuse to answer simple questions: \n97% of climate scientists agree that some amount of A in AGW is caused by humans. \nIs that the conclusion of the Cook paper, or ain't it? If the Cook paper stated a quantity for A, tell us what it is. (watch rob squirm)")