;;;; Web service for the semantic frame extractor

(in-package :frame-extractor)

(export '(frame-extractor-app* *frame-extractor-acceptor*))

(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
  (let (missing-keys)
    (loop for key in keys
          unless (assoc key json)
          do (push key missing-keys))
    missing-keys))

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
    (let ((frame-set (pie-comprehend utterance :silent silent)))
      (encode-json-alist-to-string `((:frames . ,(loop for frame in (pie::entities frame-set)
                                                 collect (encode-json-to-string frame))))
                               ))))

;;; Use hunchentoot
(defvar *frame-extractor-app* (snooze:make-hunchentoot-app))
(push *frame-extractor-app* hunchentoot:*dispatch-table*)
(defvar *frame-extractor-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9003))

(hunchentoot:start *frame-extractor-acceptor*)




;;curl -H "Content-Type: application/json" -d '{"utterance" : "Oxygen levels in oceans have fallen 2 % in 50 years due to climate change."}' http://localhost:9003/frame-extractor/extract-frames

;; {"frames":["{\"id\":\"causationFrame8\",\"utterance\":\"due to climate change\",\"frameVar\":\"?frame7\",\"frameEvokingElement\":\"due to\",\"cause\":\"climate change\",\"effect\":{\"id\":\"changePositionOnAScaleFrame5\",\"utterance\":\"Oxygen levels in oceans have fallen 2 % in 50 years due to climate change\",\"frameVar\":\"?frame25\",\"frameEvokingElement\":\"fall\",\"item\":\"oxygen levels in oceans\",\"difference\":\"2 %\",\"duration\":\"in 50 years\"},\"actor\":null,\"affected\":null}"]}