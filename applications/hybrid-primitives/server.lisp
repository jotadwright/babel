
(in-package :hybrid-primitives)

(defun load-image (endpoint image-name)
  "Load a CLEVR image before starting
   the IRL program evaluation"
  (let* ((json-data
          (encode-json-alist-to-string
           `((:name . ,image-name))))
         (response
          (multiple-value-list
           (http-request endpoint :method :post
                         :content-type "application/json"
                         :content json-data)))
         (response-code (second response)))
    (unless (= response-code 200)
      (error "Something went wrong while loading ~a" image-name))))

(defun evaluate-neural-primitive (endpoint data)
  "Evaluate a neural primitive. Check if the response
   status code is ok and return the relevant data"
  (let* ((json-data
          (encode-json-alist-to-string data))
         (response
          (multiple-value-list
           (http-request endpoint :method :post
                         :content-type "application/json"
                         :content json-data)))
         (response-code (second response))
         (response-data
          (handler-case
              (decode-json-from-string (first response))
            (error (c)
              (warn
               (format nil "Error decoding json in evaluate-neural-primitive"))))))
    (cond
     ((= response-code 400) ; something went wrong
      (let ((error-type (rest (assoc :error--type response-data)))
            (error-msg (rest (assoc :message response-data))))
        (error "~a error: ~a" error-type error-msg)))
     ((= response-code 200) ; status normal
      (cond
       ;; if bindings, return them
       ((assoc :bindings response-data)
        (rest (assoc :bindings response-data)))
       ;; if consistent, return it
       ((assoc :consistent response-data)
        (rest (assoc :consistent response-data))))))))
    