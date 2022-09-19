
(in-package :visual-dialog)

(export '(load-image request-attn))

(defun do-irl-request (server-address endpoint data cookie-jar)
  (multiple-value-bind (response code headers
                                 uri stream must-close
                                 reason-phrase)
      (http-request (mkstr server-address endpoint)
                    :method :post :content-type "application/json"
                    :content (replace-char (downcase (to-json data)) #\- #\_)
                    ;:keep-alive t
                    :connection-timeout 10
                    :cookie-jar cookie-jar)
    (declare (ignorable headers uri stream must-close reason-phrase))
    (values (parse (upcase (replace-char response #\_ #\-))) code)))


(defun load-image (server-address cookie-jar image-name)
  "Load a CLEVR image before starting
   the IRL program evaluation"
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "init-image"
                      `(:name ,image-name)
                      cookie-jar)
    (declare (ignorable response))
    (unless (= code 200)
      (error "Something went wrong while loading ~a" image-name))))

(defun clear-session (server-address cookie-jar)
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "clear_session"
                      nil cookie-jar)
    (declare (ignorable response))
    (unless (= code 200)
      (error "Something went wrong while clearing the session"))
    (when (= code 200)
      ;; the session is cleared on the server and the
      ;; cookie is set to expired and sent back.
      ;; Drakma detects this and removes the cookie
      ;; from the jar. Amazing!
      (drakma:delete-old-cookies cookie-jar))))


(defun clear-attentions (server-address cookie-jar)
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "clear-attentions"
                      nil cookie-jar)
    (declare (ignorable response))
    (unless (= code 200)
      (error "Something went wrong while clearing the attentions"))
    t))


(defun clear-scenes (server-address cookie-jar)
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "clear-scenes"
                      nil cookie-jar)
    (declare (ignorable response))
    (unless (= code 200)
      (error "Something went wrong while clearing the attentions"))
    t))

(defun evaluate-neural-primitive (primitive server-address cookie-jar data)
  "Evaluate a neural primitive. Check if the response
   status code is ok and return the relevant data"
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      primitive
                      data cookie-jar)
    (cond
     ((= code 400) ; something went wrong
      (let ((error-type (getf response :error-type))
            (error-msg (getf response :message)))
        (error "~a error: ~a" error-type error-msg)))
     ((= code 200) ; status normal
      (cond
       ;; if bindings, return them
       ((getf response :bindings)
        (process-bindings (getf response :bindings)))
       ;; if consistent, return it
       ((getf response :consistent)
        (getf response :consistent)))))))

(defun compare-attentions (server-address cookie-jar data)
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "compare_attentions"
                      data cookie-jar)
    ;(print response)
    (getf response :same-attention)))

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
  (let ((bind-scores
         (loop for bind-set in bindings
               collect (loop for bind-statement in bind-set
                             append (list
                                     (intern (getf bind-statement :variable)
                                             :visual-dialog)
                                     (getf bind-statement :score)))))
        (bind-values
         (loop for bind-set in bindings
               collect (loop for bind-statement in bind-set
                             append (list
                                     (intern (getf bind-statement :variable)
                                             :visual-dialog)
                                     (getf bind-statement :value))))))
    (values bind-scores bind-values)))


(defun request-attn (server-address cookie-jar attention)
  (multiple-value-bind (byte-array code headers
                                   uri stream must-close
                                   reason-phrase)
      (http-request (replace-char
                     (downcase
                      (mkstr server-address
                             (format nil "attn/~a"
                                     (id attention))))
                     #\- #\_)
                    :method :get
                    :cookie-jar cookie-jar)
    (declare (ignorable headers uri stream must-close reason-phrase))
    (when (= code 200)
      (let ((filepath (babel-pathname :directory '(".tmp" "attn") :type "png"
                                      :name (downcase (mkstr (id attention)))
                                      )))
        (ensure-directories-exist filepath)
        (with-open-file (stream filepath :direction :output
                                :element-type 'unsigned-byte
                                :if-exists :overwrite :if-does-not-exist :create)
          (loop for byte across byte-array
                do (write-byte byte stream)))
        (setf (img-path attention) filepath)
        filepath))))

#|(defmethod irl::handle-evaluate-irl-program-finished-event
           :before ((monitor monitors::monitor)
                    (monitor-id (eql 'irl::trace-irl))
                    (event-id (eql 'irl::evaluate-irl-program-finished))
                    solution-nodes pip)
  ;; when the monitor is active
  ;; download all attention images
  ;; also check if the slot is bound
  ;; such that the same attention is not downloaded twice
  (when (monitors::active monitor)
    (loop for pipn in solution-nodes
          do (loop for binding in (irl::bindings pipn)
                   when (and (eql (type-of (value binding)) 'attention)
                             (null (img-path (value binding))))
                   do (request-attn (get-data (ontology pip) 'visual-dialog::server-address)
                                    (get-data (ontology pip) 'visual-dialog::cookie-jar)
                                    (value binding))))))|#


