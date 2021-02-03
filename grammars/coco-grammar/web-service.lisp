;;;; web-service.lisp

(in-package :coco-web-service)

#|
 (coco-meaning->rpn (fcg:comprehend "what color is the traffic light left of the cat"))
 (coco-meaning->rpn (fcg:comprehend "what color is the leftmost traffic light"))
(coco-meaning->rpn (fcg:comprehend "is the person in the leftmost car"))
(coco-meaning->rpn (fcg:comprehend "where in the photo is the rightmost woman, on the left or on the right"))
(coco-meaning->rpn (fcg:comprehend "where in the photo is the leftmost wine glass"))
(coco-meaning->rpn (fcg:comprehend "what is the sex of the leftmost person"))
|#

;;;; Helper functions
;;;; ----------------

(defun keys-present-p (json &rest keys)
  "Check if all keys are present in the given
   json object."
  (let (missing-keys)
    (loop for key in keys
          unless (assoc key json)
          do (push key missing-keys))
    missing-keys))

(defmethod explain-condition ((condition http-condition)
                              resource
                              ct)
  (encode-json-alist-to-string
   `((:status--code . ,(format nil "~a" (status-code condition)))
     (:details . ,(apply #'format nil (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

;;;; Comprehend
;;;; ----------
(defun handle-comprehend-route (json)
  (let* ((missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json)))
         (meaning-representation
          (if (assoc :meaning--representation json)
            (downcase (rest (assoc :meaning--representation json)))
            "irl"))
         (want-svg-p
          (rest (assoc :want--svg json))))
    ;; signal an error when keys are missing
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})"
                      missing-keys))
    ;; signal an error when utterance is not a string
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string."
                      (type-of utterance)))
    ;; signal an error when meaning-representation is
    ;; not 'irl' or 'rpn'
    (unless (or (string= meaning-representation "irl")
                (string= meaning-representation "rpn"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'irl' or 'rpn'."
                      meaning-representation))
    ;; comprehend the utterance
    ;; signal an error when something goes wrong
    (multiple-value-bind (irl-program cipn)
        (handler-case (fcg:comprehend utterance :cxn-inventory *COCO* :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      ;; return the meaning, the fcg status, the applied constructions
      ;; and the constructional dependencies in SVG
      (let ((fcg-status (first (statuses cipn)))
            (svg (when want-svg-p
                   (let* ((unit-bindings (analyse-solution cipn '<-))
                          (s-dot (unit-bindings->graph :data unit-bindings
                                                       :construction-inventory *COCO*
                                                       :prefered-font "Arial")))
                     (s-dot->svg s-dot)))))
        (encode-json-alist-to-string
         `((:meaning . ,(when (and irl-program
                                   (eql fcg-status 'fcg::succeeded))
                          (cond
                           ((string= meaning-representation "irl")
                            (downcase (mkstr irl-program)))
                           ((string= meaning-representation "rpn")
                            (coco-meaning->rpn irl-program)))))
           (:fcg--status . ,(downcase (mkstr fcg-status)))
           (:applied--constructions . ,(when (applied-constructions cipn)
                                         (mapcar #'downcase
                                                 (mapcar #'mkstr
                                                         (mapcar #'fcg::name
                                                                 (applied-constructions cipn))))))
           (:svg . ,svg)))))))
     
;; send a request to /comprehend
;; data must contain an "utterance" key
;; optionally, "meaning_representation" can be specified
;; this can be set to "irl" or "rpn"
;; finally, there is the "want_svg" argument. This
;; must be true of false (the json values, not strings).
;; Depending on this, the SVG of the constructional
;; dependencies is sent back or not
(defroute comprehend (:post :application/json)
 (handle-comprehend-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))
         
(defroute comprehend (:post :text/plain)
  (handle-comprehend-route
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 "Malformed JSON")))))


;; curl -H "Content-Type: text/plain" -d '{"utterance" : "where is the cow?", "meaning_representation": "rpn", "want_svg": true}' http://localhost:9009/comprehend