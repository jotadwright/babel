
(in-package :hybrid-primitives)

(export '(load-image))

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
        (process-bindings
         (rest (assoc :bindings response-data))))
       ;; if consistent, return it
       ((assoc :consistent response-data)
        (rest (assoc :consistent response-data))))))))

(defun process-bindings (bindings)
  "Process the new binding such that they are easier
   to handle in the primitive definition. The variable
   should always refer to some internal symbol (i.e.
   a slot spec of the primitive), the score is always
   a float between 0 and 1 and the value can be anything,
   so it is left as is. Is is up to the primitive definition
   to further process the value.
   The new bindings are returned as a list of lists of dictionaries.
   This is because multiple variables can be bound in 1 go and
   the same variable can be bound multiple times."
  (loop for bind-set in bindings
        collect (loop for bind-statement in bind-set
                      collect (list
                               (internal-symb (upcase (rest (assoc :variable bind-statement))))
                               (parse-float (rest (assoc :score bind-statement)))
                               (rest (assoc :value bind-statement))))))
    