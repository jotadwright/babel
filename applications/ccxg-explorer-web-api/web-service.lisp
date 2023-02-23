
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

;; curl -H "Content-Type: application/json" -d '{"corpus" : "ontonotes", "maxN":"100", "orderMatters":"T", "schema": [{"roleType":"arg0"},{"roleType":"v","roleset":"explain.01"},{"roleType":"arg2"},{"roleType":"arg1"}]}' http://localhost:8500/by-schema


