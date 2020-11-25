(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Requesting a probabability distribution over the construction inventory  ;;
;; for the the next construction to apply                                   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun seq2seq-next-cxn (utterance/meaning applied-cxns model endpoint
                                   &key (number-cutoff nil) (probability-cutoff nil))
  "Queries the seq2seq model for a probability distribution for the next cxn to apply."
  (let* ((applied-cxns (if applied-cxns
                         ;; the cxn names should not be transformed to camelcase
                         (mapcar (compose #'mkstr #'name) applied-cxns)
                         ;; if no applied cxns an empty vector will transform into [ ] (instead of '() into null)
                         #()))
         (json (cl-json:encode-json-alist-to-string `((:model . ,model)
                                                      (:utterance/meaning . ,utterance/meaning)
                                                      (:applied--cxns . ,applied-cxns)
                                                      (:number--cutoff . ,number-cutoff)
                                                      (:probability--cutoff . ,probability-cutoff))))
         (response-string (drakma:http-request endpoint
                                               :method :post
                                               :content-type "application/json"
                                               :content json
                                               #+sbcl :connection-timeout nil
                                               ))
                          ;;#+ccl (dex:post endpoint :content json :headers '((Content-Type . "application/json"))))
         (response-object (handler-case (cl-json:decode-json-from-string response-string)
                            (error (c) (warn (format nil "Error decoding json in seq2seq-next-cxn."))))))
    (when response-object
      (assqv :probabilities response-object))))
    


