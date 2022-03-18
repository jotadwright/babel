;;;; /comprehend-all route
(defroute comprehend-all (:post :application/json)
  (let* ((json
          (handler-case
              (decode-json-from-string
               (payload-as-string))
            (error (e)
              (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json)))
         (n (when (assoc :n json) (rest (assoc :n json))))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr")))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})"
                      missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string."
                      (type-of utterance)))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json")
                (string= irl-encoding "rpn"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'."
                      irl-encoding))
    (multiple-value-bind (irl-programs cipns)
        (handler-case (fcg:comprehend-all (preprocess-sentence utterance)
                                          :cxn-inventory *CLEVR*
                                          :silent t
                                          :n n)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-to-string
       (loop for irl-program in irl-programs
             for cipn in cipns
             collect `((:meaning . ,(when irl-program
                                      (cond
                                        ((string= irl-encoding "sexpr")
                                         (mkstr irl-program))
                                        ((string= irl-encoding "json")
                                         (encode-irl-program-as-json irl-program nil))
                                        ((string= irl-encoding "rpn")
                                         (encode-irl-program-as-rpn irl-program)))))
                       (:fcg--status . ,(mkstr (first (statuses cipn))))
                       (:applied--constructions . ,(when (applied-constructions cipn)
                                                     (mapcar #'downcase
                                                             (mapcar #'mkstr
                                                                     (mapcar #'fcg::name
                                                                             (applied-constructions cipn))))))))))))


;;;; /formulate route
(defroute formulate (:post :application/json)
  (let* ((json
          (handler-case
              (decode-json-from-string
               (payload-as-string))
            (error (e)
              (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :meaning))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr"))
         (irl-program (cond
                       ((string= irl-encoding "sexpr")
                        (read-from-string (rest (assoc :meaning json))))
                       ((string= irl-encoding "json")
                        (decode-irl-program
                         (rest (assoc :meaning json)))))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    ;; ccl requires to intern the symbols manually (why?)
    #+ccl (setf irl-program
                (loop for predicate in irl-program
                   collect (loop for symbol in predicate
                              collect (intern (mkstr symbol) 'clevr))))
    (handler-case
        (unless (irl-program-p irl-program)
          (http-condition 400 "Invalid IRL program: (~a)" irl-program))
      (error (e)
        (http-condition 400 "Unbound symbol in IRL program: (~a)" irl-program)))
    (multiple-value-bind (utterance cipn)
        (handler-case (fcg:formulate irl-program
                                     :cxn-inventory *CLEVR*
                                     :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-alist-to-string
       `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
         (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
         (:applied--constructions . ,(when (applied-constructions cipn)
                                       (mapcar #'downcase
                                               (mapcar #'mkstr
                                                       (mapcar #'fcg::name
                                                               (applied-constructions cipn)))))))))))

;;;; /formulate-all route
(defroute formulate-all (:post :application/json)
  (let* ((json (handler-case
                   (decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :meaning))
         (n (when (assoc :n json) (rest (assoc :n json))))
         (irl-encoding (if (assoc :irl--encoding json)
                         (downcase (rest (assoc :irl--encoding json)))
                         "sexpr"))
         (irl-program (cond
                    ((string= irl-encoding "sexpr")
                     (read-from-string (rest (assoc :meaning json))))
                    ((string= irl-encoding "json")
                     (decode-irl-program
                      (rest (assoc :meaning json)))))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (or (string= irl-encoding "sexpr")
                (string= irl-encoding "json"))
      (http-condition 400 "Invalid irl-encoding specified: ~a. Expected 'sexpr' or 'json'." irl-encoding))
    ;; ccl requires to intern the symbols manually (why?)
    #+ccl (setf irl-program
                (loop for predicate in irl-program
                   collect (loop for symbol in predicate
                              collect (intern (mkstr symbol) 'clevr))))
    (handler-case
        (unless (irl-program-p irl-program)
          (http-condition 400 "Invalid IRL program: (~a)" irl-program))
      (error (e)
        (http-condition 400 "Unbound symbol in IRL program: (~a)" irl-program)))
    (multiple-value-bind (utterances cipns)
        (handler-case (fcg:formulate-all irl-program
                                         :cxn-inventory *CLEVR*
                                         :silent t
                                         :n n)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-to-string
       (loop for utterance in utterances
             for cipn in cipns
             collect `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
                       (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
                       (:applied--constructions . ,(when (applied-constructions cipn)
                                                     (mapcar #'downcase
                                                             (mapcar #'mkstr
                                                                     (mapcar #'fcg::name
                                                                             (applied-constructions cipn))))))))))))


;;;; /comprehend-and-formulate route
(defroute comprehend-and-formulate (:post :application/json)
  (let* ((json (handler-case
                   (decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON"))))
         (missing-keys (keys-present-p json :utterance))
         (utterance (rest (assoc :utterance json))))
    (when missing-keys
      (http-condition 400 "JSON input missing key(s): (~{~a~^, ~})" missing-keys))
    (unless (stringp utterance)
      (http-condition 400 "utterance is of type ~a. Expected something of type string." (type-of utterance)))
    (multiple-value-bind (utterance cipn)
        (handler-case (fcg:comprehend-and-formulate (preprocess-sentence utterance)
                                                    :cxn-inventory *CLEVR*
                                                    :silent t)
          (error (e)
            (http-condition 500 "Error in language processing module!" e)))
      (encode-json-alist-to-string
       `((:utterance . ,(when utterance (mkstr (list-of-strings->string utterance))))
         (:fcg--status . ,(downcase (mkstr (first (statuses cipn)))))
         (:applied--constructions . ,(when (applied-constructions cipn)
                                       (mapcar #'downcase
                                               (mapcar #'mkstr
                                                       (mapcar #'fcg::name
                                                               (applied-constructions cipn)))))))))))

