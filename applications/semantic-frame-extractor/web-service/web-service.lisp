;;;; Web service for the semantic frame extractor

(in-package :frame-extractor)

(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
  (let (missing-keys)
    (loop for key in keys
          unless (assoc key json)
          do (push key missing-keys))
    missing-keys))
#|
(defmethod encode-json ((frame frame)
                        &optional (stream *json-output*))
  "Overwrite of encode-json for a frame"
  (with-object (stream)
    (cl-mop:map-slots (lambda (key value)
                        (encode-object-member
                         (internal-symb (string-replace (mkstr key) "-" "--"))
                         (when value (mkstr value))
                         stream))
                      frame)))
|#

(defmethod snooze:explain-condition ((condition snooze:http-condition)
                                     resource
                                     ct)
  "Overload the explain-condition method to provide clearer error handling
   to the user of the API. A JSON object with status-code and error message
   will be send back."
  (encode-json-to-string
   `((:status--code . ,(format nil "~a" (snooze:status-code condition)))
     (:details . ,(apply #'format nil (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

(snooze:defroute frame-extractor (:post :application/json (op (eql 'extract-frames)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (snooze:http-condition 400 "Utterance is not a string! Instead, received something of type ~a" (type-of utterance)))
    (let ((frame-set (pie-comprehend utterance :silent silent)))
      (encode-json-alist-to-string
       `((:frames . ,(loop for frame in (pie::entities frame-set)
                           collect frame)))))))




;; curl -H "Content-Type: application/json" -d '{"utterance" : "Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs."}' http://localhost:9003/frame-extractor/extract-frames

;; {"frames":["{\"id\":\"causationFrame8\",\"utterance\":\"due to climate change\",\"frameVar\":\"?frame7\",\"frameEvokingElement\":\"due to\",\"cause\":\"climate change\",\"effect\":{\"id\":\"changePositionOnAScaleFrame5\",\"utterance\":\"Oxygen levels in oceans have fallen 2 % in 50 years due to climate change\",\"frameVar\":\"?frame25\",\"frameEvokingElement\":\"fall\",\"item\":\"oxygen levels in oceans\",\"difference\":\"2 %\",\"duration\":\"in 50 years\"},\"actor\":null,\"affected\":null}"]}