
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; CCxG explorer web service    ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :propbank-grammar)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      ROUTE QUERY BY SCHEMA       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(snooze:defroute ccxg-explorer-api (:post :application/json (op (eql 'by-schema)))
(snooze:defroute by-schema (:post :application/json)
  (let* ((json (handler-case
                   (cl-json:decode-json-from-string
                    (snooze:payload-as-string))
                 (error (e)
                   (snooze:http-condition 400 "Malformed JSON (~a)!" e))))
         (missing-keys (keys-present-p json :schema :order-matters :max-n :corpus :allow-additional-roles))
         (schema (rest (assoc :schema json)))
         (order-matters (rest (assoc :order-matters json)))
         (max-n (rest (assoc :max-n json)))
         (corpus (rest (assoc :corpus json)))
         (allow-additional-roles (rest (assoc :allow-additional-roles json))))
    (when missing-keys
      (snooze:http-condition 400 "JSON missing key(s): ({~a~^, ~})" missing-keys))
    (pprint allow-additional-roles)
    (when (and order-matters allow-additional-roles)
      (snooze:http-condition 400 "JSON can not simultaneously have order-matters and additional-roles-allowed"))
    (let ((results (handler-case (find-by-schema (make-kw corpus)
                                                 (transform-schema schema order-matters allow-additional-roles)
                                                 :max-n (parse-integer max-n))
                       (error (e)
                         (snooze:http-condition 500 "Error in construction explorer API!" e)))))
      (cl-json:encode-json-alist-to-string
       (transform-results results)))))


;; OPTIONS preflight check
(snooze:defroute by-schema(:options :text/*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ROUTE EXTRACT FRAMES         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(snooze:defroute extract-frames-route (:post :application/json)
  (utterance-extract-frames (handler-case
                (cl-json::decode-json-from-string (snooze:payload-as-string))
              (error ()
                (snooze:http-condition 400 "Malformed JSON!")))))

;; (defun testfunc (json-input)
;;   (cl-json:encode-json-alist-to-string json-input))

(defun check-for-missing-keys (json-input required-keys)
  (let ((missing-keys (loop for key in required-keys
                            if (not (assoc key json-input))
                              collect key)))
    (when missing-keys
      (snooze:http-condition 400 (format nil "Please provide values for the
following missing key(s): ~{\'~(~a~)\'~^, ~}." missing-keys)))))


(defun utterance-extract-frames (json-input)
  (check-for-missing-keys json-input '(:utterance :package :grammar :timeout))
  (let* ((utterance-raw (cdr (assoc :utterance json-input)))
         (utterance (if (stringp utterance-raw)
                        utterance-raw
                        (http-condition 400 (format nil
                                                    "Utterance should be of type string. Received a ~a."
                                                    (type-of utterance-raw)))))
         (package-raw (cdr (assoc :package json-input)))
         (package (if (find-package (utils:make-kw package-raw))
                      (find-package (utils:make-kw package-raw))
                      (http-condition 400 (format nil "Package '~a' not found." package-raw))))
         (grammar-raw (cdr (assoc :grammar json-input)))
         (grammar (if (find-symbol (utils:upcase grammar-raw) package)
                      (symbol-value (find-symbol (utils:upcase grammar-raw) package))
                      (http-condition 400 (format nil
                                                  "Grammar '~a' not found in package '~a'."
                                                  grammar-raw package-raw))))
         (timeout-raw (cdr (assoc :timeout json-input)))
         (timeout (if (numberp timeout-raw)
                      timeout-raw
                      (snooze:http-condition 400 (format nil
                                                  "Timeout should be of type number. Received '~a'."
                                                  (type-of timeout-raw))))))
    (handler-case
        (trivial-timeout:with-timeout (timeout)
          (multiple-value-bind (solution cipn frame-set)
              (handler-case (monitors:with-disabled-monitor-notifications
                              (utils::with-package (package-name package)
                                (propbank-grammar::comprehend-and-extract-frames utterance :cxn-inventory grammar :silent t)))
                (error (e)
                  (snooze:http-condition 400 (format nil "Error during the comprehension process: ~a" e))))
            (declare (ignore solution cipn))
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
        (snooze:http-condition 500 "Timeout exceeded!")))))


;; OPTIONS preflight check
(snooze:defroute extract-frames-route (:options :text/*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               UTILITIES          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun transform-schema (schema order-matters allow-additional-roles)
  (loop for role in schema
        collect (loop for (role-part . role-value) in role
                      collect (cond ((eq role-part :string)
                                     (list role-part role-value))
                                    ((eq role-part :pos)
                                     (list role-part
                                     (append '(==) (mapcar #'intern (mapcar #'upcase role-value)))))
                                    (t
                                     (list role-part (intern (upcase role-value)))))
                      into roles
                      finally (return `(==1 ,@roles)))
        into transformed-roles
        finally (return (cond (order-matters transformed-roles)
                              (allow-additional-roles (append (list '==) transformed-roles))
                              (t (append (list '==p) transformed-roles))))))

(defun transform-results (results)
  `((:results . ,(loop for result in results collect (transform-result result)))))

(defun transform-result (result)
  (let ((utterance (second (first result)))
        (roles (second (second result))))
    `((:utterance . ,utterance)
      (:roles . ,(loop for role in roles collect (transform-role role))))))

(defun transform-role (role)
  (loop for role-part in role
        collect (cons (first role-part) (second role-part))))

;; curl -H "Content-Type: application/json" -d '{"corpus" : "ontonotes", "maxN":"100", "orderMatters":"T", "allowAdditionalRoles":null, "schema": [{"roleType":"arg0"},{"roleType":"v","roleset":"explain.01"},{"roleType":"arg2"},{"roleType":"arg1"}]}' http://localhost:8500/by-schema

;; curl http://localhost:8500/extract-frames -H "Content-Type: application/json" -d '{"utterance":"He told him a story", "package": "propbank-grammar", "grammar": "*restored-grammar*", "timeout": 100}'
