
(ql:quickload :propbank-english)
(ql:quickload :corpus-processing)
(ql:quickload :cl-csv)

(in-package :propbank-english)

; (activate-monitor trace-fcg)

(defparameter *restored-100-grammar*
  (restore
   (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-cleaned-100"
                   :type "fcg")))

(defparameter *inputfile*
   (make-pathname :directory '(:absolute "Users" "jensnevens" "Projects")
                  :name "tweets_economists" :type "csv"))

(defparameter *outputfile*
  (make-pathname :directory '(:absolute "Users" "jensnevens" "Projects")
                 :name "annotated_tweets_economists" :type "json"))

(defun split-sentences (text)
  (remove-if #'(lambda (s) (= (length s) 0))
             (mapcar #'remove-spurious-spaces
                     (flatten
                      (mapcar #'(lambda (sentences)
                                  (split sentences #\!))
                              (flatten
                               (mapcar #'(lambda (sentences)
                                           (split sentences #\?))
                                       (split text #\.))))))))


(defun process-tweet (line &key (cxn-inventory *fcg-constructions*))
  (let* ((fields (cl-csv:read-csv-row line))
         (index
          (parse-integer (first fields)))
         (utterances
          (split-sentences (second fields))))
    (loop for utterance in utterances
          for (solution cipn frame-set)
          = (multiple-value-list
             (comprehend-and-extract-frames
              utterance :silent t
              :cxn-inventory cxn-inventory))
          if (eql solution 'time-out)
          collect
          `((:frame-set . nil) (utterance . ,utterance)) into frame-sets
          else collect
          `((:frame-set  . ,(loop for frame in (frames frame-set)
                                  collect `((:frame-name . ,(frame-name frame))
                                            (:roles . ,(append
                                                        `(((:role . "V")
                                                           (:string . ,(fel-string (frame-evoking-element frame)))
                                                           (:indices . ,(indices (frame-evoking-element frame)))))
                                                        (loop for fe in (frame-elements frame)
                                                              collect `((:role . ,(fe-role fe))
                                                                        (:string . ,(fe-string fe))
                                                                        (:indices . ,(indices fe)))))))))
            (:utterance . ,utterance)) into frame-sets
          finally
          (return
           (cl-json:encode-json-alist-to-string
            `((:index . ,index)
              (:frame-sets . ,frame-sets)))))))


(defun process-tweet-corpus ()
  (process-corpus-with-threads
   :function #'process-tweet
   :function-kwargs (list :cxn-inventory *restored-100-grammar*)
   :inputfile *inputfile*
   :outputfile *outputfile*
   :number-of-threads 50
   :number-of-lines-per-thread 850
   :write-empty-lines-p t))

(process-tweet-corpus)

