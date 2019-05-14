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

(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'extract-frames)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :utterance :frames))
         (utterance (rest (assoc :utterance json)))
         (frames (rest (assoc :frames json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (snooze:http-condition 400 "Utterance is not a string! Instead, received something of type ~a" (type-of utterance)))
    
    (load-frames frames)
    
    (let ((frame-set (handler-case (pie-comprehend utterance :silent silent :cxn-inventory *fcg-constructions*)
                       (error (e)
                         (snooze:http-condition 500 "Error in precision language processing module!" e)))))
      (encode-json-alist-to-string
       `((:frame-set . ,(loop for frame in (pie::entities frame-set)
                           collect frame)))))))

(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'texts-extract-frames)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :texts :frames))
         (texts (rest (assoc :texts json)))
         (frames (rest (assoc :frames json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp texts)
      (snooze:http-condition 400 "Texts is not a list! Instead, received something of type ~a" (type-of texts)))
    
    (load-frames frames)

    (let ((text-frame-sets (loop for text in texts
                                 for utterances = (get-penelope-sentence-tokens text)
                                 collect (loop for utterance in utterances
                                               for frame-set = (handler-case (pie-comprehend utterance :silent silent :cxn-inventory *fcg-constructions*)
                                                                 (error (e)
                                                                   (snooze:http-condition 500 "Error in precision language processing module!" e)))
                                               when frame-set
                                               collect it))))
      
      (encode-json-alist-to-string
       `((:frame-sets . ,text-frame-sets))))))


(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'texts-extract-causes-effects)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :texts))
         (texts (rest (assoc :texts json)))
         (silent (if (assoc :silent json) (rest (assoc :silent json)) t)))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (listp texts)
      (snooze:http-condition 400 "Texts is not a list! Instead, received something of type ~a" (type-of texts)))
    
    (load-frames '("Causation"))

    (let* ((text-frame-sets (loop for text in texts
                                  for utterances = (get-penelope-sentence-tokens text)
                                  append (loop for utterance in utterances
                                               for frame-set = (handler-case (pie-comprehend utterance :silent silent :cxn-inventory *fcg-constructions*)
                                                                 (error (e)
                                                                   (snooze:http-condition 500 "Error in precision language processing module!" e)))
                                               when frame-set
                                               collect it)))
           (utterances-with-causes-and-effects (loop for frameset in text-frame-sets
                                                     for utterance = (utterance frameset)
                                                     append (loop for entity in (pie::entities frameset)
                                                                   collect `((:utterance . ,utterance)
                                                                             (:cause . ,(cause entity))
                                                                             (:effect . ,(effect entity)))))))
      
      (encode-json-alist-to-string
       `((:causal-relations . ,utterances-with-causes-and-effects))))))


(snooze:defroute semantic-frame-extractor (:post :application/json (op (eql 'causation-tracker)))
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :phrase :direction :data))
         (phrase (rest (assoc :phrase json)))
         (direction (rest (assoc :direction json)))
         (data (rest (assoc :data json))))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (unless (stringp phrase)
      (snooze:http-condition 400 "Phrase is not a string! Instead, received something of type ~a" (type-of phrase)))
    (unless (stringp direction)
      (snooze:http-condition 400 "Direction is not a string! Instead, received something of type ~a" (type-of direction)))
    (unless (stringp data)
      (snooze:http-condition 400 "Data is not a string! Instead, received something of type ~a" (type-of data)))
    (if (equalp direction "cause->effect")
      (cause->effect-graph phrase data)
      (effect->cause-graph phrase data))))

(snooze:defroute semantic-frame-extractor (:options :text/* (op (eql 'causation-tracker))))


;; curl -H "Content-Type: application/json" -d '{"texts" : ["Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs. This causes that.", "This is a sentence. This causes that."]}' http://localhost:9004/semantic-frame-extractor/texts-extract-causes-effects

;; {"frameSets":[[[{"id":"causationFrame15","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame30","frameEvokingElement":"cause","cause":"they","effect":"damage to their own clothes","actor":null,"affected":null}],[{"id":"causationFrame16","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]],[[{"id":"causationFrame17","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]]]}



;; curl -H "Content-Type: application/json" -d '{"utterance" : "Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs caused by people.", "frames" : ["Causation"]}' http://localhost:9004/semantic-frame-extractor/extract-frames
;; {"frameSet":[{"id":"causationFrame4","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame8","frameEvokingElement":"cause","cause":"the company","effect":"damage to their own clothes","actor":null,"affected":null},]}



;; curl -H "Content-Type: application/json" -d '{"texts" : ["Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs. This causes that.", "This is a sentence. This causes that."], "frames" : ["Causation"]}' http://localhost:9004/semantic-frame-extractor/texts-extract-frames
;; {"frameSets":[[[{"id":"causationFrame15","utterance":"if they had caused damage to their own clothes at work","frameVar":"?frame30","frameEvokingElement":"cause","cause":"they","effect":"damage to their own clothes","actor":null,"affected":null}],[{"id":"causationFrame16","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]],[[{"id":"causationFrame17","utterance":"This causes that","frameVar":"?frame30","frameEvokingElement":"cause","cause":"this","effect":"that","actor":null,"affected":null}]]]}



;;Testing on AI Lab server:

;; curl -H "Content-Type: application/json" -d '{"texts" : ["With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels."], "frames" : ["Causation"]}' https://penelope.vub.be/semantic-frame-extractor/texts-extract-frames

