
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web service for the PropBank-English grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :propbank-english)

(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
    (loop for key in keys
          unless (assoc key json)
          collect key))

(defmethod snooze:explain-condition ((condition snooze:http-condition)
                                     resource
                                     ct)
  "Overload the explain-condition method to provide clearer error handling
   to the user of the API. A JSON object with status-code and error message
   will be send back."
  (cl-json:encode-json-to-string
   `((:status--code . ,(format nil "~a" (snooze:status-code condition)))
     (:details . ,(apply #'format nil (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

(snooze:defroute propbank-frame-extractor (:post :application/json (op (eql 'extract-frames-string)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json))))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (snooze:http-condition 400 "Utterance is not a string! Instead, received something of type ~a" (type-of utterance)))
        
    (let ((frame-set (handler-case (comprehend-and-extract-frames utterance :silent t :cxn-inventory *propbank-opinion-grammar*)
                       (error (e)
                         (snooze:http-condition 500 "Error in precision language processing module!" e)))))
      (cl-json:encode-json-alist-to-string
       `((:frame-set . ,(loop for frame in (frames frame-set)
                           collect `((:frame-name . ,(frame-name frame))
                                     (:roles . ,(append `(((:role . "V")
                                                          (:string . ,(fel-string (frame-evoking-element frame)))
                                                          (:indices . ,(list (index (frame-evoking-element frame))))))
                                                        (loop for fe in (frame-elements frame)
                                                             collect `((:role . ,(fe-role fe))
                                                                        (:string . ,(fe-string fe))
                                                                        (:indices . ,(indices fe))))))))))))))

(snooze:defroute propbank-frame-extractor (:post :application/json (op (eql 'extract-frames-list)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :utterances))
         (utterances (rest (assoc :utterances json))))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp utterances)
      (snooze:http-condition 400 "Utterances is not a list Instead, received something of type ~a" (type-of utterances)))

    (loop for utterance in utterances
          for frame-set = (handler-case (comprehend-and-extract-frames utterance :silent t :cxn-inventory *propbank-opinion-grammar*)
                            (error (e)
                              (snooze:http-condition 500 "Error in precision language processing module!" e)))
          collect
          `((:frame-set  . ,(loop for frame in (frames frame-set)
                                 collect `((:frame-name . ,(frame-name frame))
                                           (:roles . ,(append `(((:role . "V")
                                                                 (:string . ,(fel-string (frame-evoking-element frame)))
                                                                 (:indices . ,(list (index (frame-evoking-element frame))))))
                                                              (loop for fe in (frame-elements frame)
                                                                    collect `((:role . ,(fe-role fe))
                                                                              (:string . ,(fe-string fe))
                                                                              (:indices . ,(indices fe)))))))))
            (:utterance . ,utterance))
          into frame-sets
          finally
          return
          (cl-json:encode-json-alist-to-string
           `((:frame-sets . ,frame-sets))))))


                                                   
(defparameter *propbank-opinion-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                           :name "opinion-grammar-ontonotes"
                           :type "fcg")))







;; curl -H "Content-Type: application/json" -d '{"utterance" : "I believe in you."}' http://localhost:9007/propbank-frame-extractor/extract-frames-string


;; curl -H "Content-Type: application/json" -d '{"utterances" : ["I believe in you.", "I think that you believe in me."]}' http://localhost:9007/propbank-frame-extractor/extract-frames-list


