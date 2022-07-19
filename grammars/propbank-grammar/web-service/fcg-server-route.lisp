(in-package :fcg-server)
;(use-package :propbank-grammar)

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

(defroute extract-frames (:post :application/json)
  (fcg-server-comprehend-and-extract-frames
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error ()
       (http-condition 400 "Malformed JSON!")))))

(defun fcg-server-comprehend-and-extract-frames (json-input)
  "Handles the comprehend-utterance route."
  
  ;; 1. Assert that all required keys are present
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (check-for-missing-keys json-input '(:utterance :package :grammar :timeout))

  ;; 2. Retrieve utterance, package and grammar
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let* (;; Retrieve the utterance and assert that it is a string
         (utterance-raw (cdr (assoc :utterance json-input)))
         (utterance (if (stringp utterance-raw)
                        utterance-raw
                        (http-condition 400 (format nil "Utterance should be of type string. Received a ~a." (type-of utterance-raw)))))
         ;; Retrieve the package and assert that is found
         (package-raw (cdr (assoc :package json-input)))
         (package (if (find-package (utils:make-kw package-raw))
                      (find-package (utils:make-kw package-raw))
                      (http-condition 400 (format nil "Package '~a' not found." package-raw))))
         ;; Retrieve the grammar and assert that it is found
         (grammar-raw (cdr (assoc :grammar json-input)))
         (grammar (if (find-symbol (utils:upcase grammar-raw) package)
                      (symbol-value (find-symbol (utils:upcase grammar-raw) package))
                      (http-condition 400 (format nil "Grammar '~a' not found in package '~a'." grammar-raw package-raw))))
         ;; Retrieve the timeout and check if it is a number
         (timeout-raw (cdr (assoc :timeout json-input)))
         (timeout (if (numberp timeout-raw)
                      timeout-raw
                      (http-condition 400 (format nil "Timeout should be of type number. Received '~a'." (type-of timeout-raw))))))

    ;; 3. Perform the actual comprehension process
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (handler-case
        (trivial-timeout:with-timeout (timeout)
          (multiple-value-bind (solution cipn frame-set)
              (handler-case (monitors:with-disabled-monitor-notifications
                              (utils::with-package (package-name package)
                                (propbank-grammar::comprehend-and-extract-frames utterance :cxn-inventory grammar :silent t)))
                
                (error (e)
                  (http-condition 400 (format nil "Error during the comprehension process: ~a" e))))

                                        ;(pprint (fcg::name (fcg::construction-inventory cipn)))
            (declare (ignore solution cipn))
            ;;  4. Return the result as a json object
            (cl-json:encode-json-alist-to-string
             `((:status-code . 200)
               (:frame-set . ,(loop for frame in (propbank-grammar::frames frame-set)
                                    collect `((:frame-name . ,(propbank-grammar::frame-name frame))
                                              (:roles . ,(append `(((:role . "V")
                                                                    (:string . ,(propbank-grammar::fel-string
                                                                                 (propbank-grammar::frame-evoking-element frame)))
                                                                    (:indices . ,(propbank-grammar::indices
                                                                                  (propbank-grammar::frame-evoking-element frame)))))
                                                                 (loop for fe in (propbank-grammar::frame-elements frame)
                                                                       collect `((:role . ,(propbank-grammar::fe-role fe))
                                                                                 (:string . ,(propbank-grammar::fe-string fe))
                                                                                 (:indices . ,(propbank-grammar::indices fe)))))))))))))
      (trivial-timeout:timeout-error ()
        (http-condition 500 "Timeout exceeded!")))))
